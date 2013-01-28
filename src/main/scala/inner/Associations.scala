package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import org.squeryl.dsl.ast.{LogicalBoolean, EqualityExpression}
import squeryl.Implicits._
import ReflectionUtil._

trait Associations {
  trait Association[O <: ActiveRecordBase[_], T <: ActiveRecordBase[_]] {
    val owner: O
    val associationClass = manifest.erasure
    val manifest: Manifest[T]

    def relation: ActiveRecord.Relation[T, T]

    protected lazy val companion = classToCompanion(associationClass)
      .asInstanceOf[ActiveRecordBaseCompanion[_, T]]

    protected lazy val source =
      ActiveRecord.Relation(companion.table, {m: T => m})(manifest)

    protected[inner] def fieldInfo(name: String) = companion.fieldInfo.getOrElse(name,
      throw ActiveRecordException.notFoundField(name)) 
  }

  class BelongsToAssociation[O <: ActiveRecordBase[_], T <: ActiveRecordBase[_]](
    val owner: O, foreignKey: String
  )(implicit val manifest: Manifest[T]) extends Association[O, T] {
    lazy val fieldInfo = owner._companion.fieldInfo(foreignKey)

    def condition: T => LogicalBoolean = {
      m => fieldInfo.toEqualityExpression(m.id, owner.getValue(foreignKey))
    }

    def relation1: ActiveRecord.Relation1[T, T] = source.where(condition).limit(1)

    def relation = relation1

    def get: Option[T] = relation.headOption

    def assign(m: T): T = {
      fieldInfo.setValue(owner, m.id)
      m
    }

    def associate(m: T): T = {
      val t = assign(m)
      t.save
      t
    }

    def :=(m: T): T = assign(m)
  }

  class HasManyAssociation[O <: ActiveRecordBase[_], T <: ActiveRecordBase[_]](
    val owner: O, conditions: Map[String, Any], foreignKey: String
  )(implicit val manifest: Manifest[T]) extends Association[O, T] {
    private def allConditions = conditions + (foreignKey -> owner.id)

    def condition: T => LogicalBoolean = {
      m => LogicalBoolean.and(allConditions.map {
        case (key, value) =>
          fieldInfo(key).toEqualityExpression(m.getValue(key), value)
      }.toSeq)
    }

    def relation1: ActiveRecord.Relation1[T, T] = source.where(condition)

    def relation = relation1

    def build: T = assign(companion.newInstance)

    def assign(m: T): T = {
      allConditions.foreach {
        case (key, value) => fieldInfo(key).setValue(m, value)
      }
      m
    }

    def associate(m: T): T = inTransaction {
      val t = assign(m)
      t.save
      t
    }

    def <<(m: T): T = associate(m)

    def :=(list: List[T]): List[T] = {
      deleteAll
      list.map(associate)
    }

    def deleteAll(): List[T] = inTransaction {
      val result = relation.toList
      result.foreach(_.delete)
      result
    }
  }

  class HasManyThroughAssociation[O <: ActiveRecordBase[_], T <: ActiveRecordBase[_], I <: ActiveRecordBase[_]](
    val owner: O, val through: HasManyAssociation[O, I],
    conditions: Map[String, Any], foreignKey: String
  )(implicit val manifest: Manifest[T], m: Manifest[I]) extends Association[O, T] {
    def relation2: ActiveRecord.Relation2[T, I, T] = source.joins[I]{
      (m, inter) =>
        val f = fieldInfo("id")
        val e1 = f.toExpression(m.id)
        val e2 = f.toExpression(inter.getValue(foreignKey))
        new EqualityExpression(e1, e2)
    }.where(
      (m, inter) =>
      LogicalBoolean.and(through.condition(inter) :: conditions.map {
        case (key, value) =>
          fieldInfo(key).toEqualityExpression(m.getValue(key), value)
      }.toList)
    )

    def relation = relation2

    def assign(m: T): I = {
      conditions.foreach {
        case (key, value) => fieldInfo(key).setValue(m, value)
      }
      val inter = through.build
      through.fieldInfo(foreignKey).setValue(inter, m.id)
      inter
    }

    def associate(m: T): I = inTransaction {
      val i = assign(m)
      i.save
      i
    }

    def <<(m: T): I = associate(m)

    def :=(list: List[T]): List[I] = {
      deleteAll
      list.map(associate)
    }

    def deleteAll(): List[T] = inTransaction {
      through.deleteAll
      val result = relation.toList
      result.foreach(_.delete)
      result
    }
  }

  trait AssociationSupport { self: ActiveRecordBase[_] =>
    protected def belongsTo[T <: ActiveRecordBase[_]]
      (implicit m: Manifest[T]): BelongsToAssociation[this.type, T] =
        belongsTo[T](Config.schema.foreignKeyFromClass(m.erasure))
          .asInstanceOf[BelongsToAssociation[this.type, T]]

    protected def belongsTo[T <: ActiveRecordBase[_]](foreignKey: String)
      (implicit m: Manifest[T]): BelongsToAssociation[this.type, T] =
        new BelongsToAssociation[this.type, T](self, foreignKey)

    protected def hasMany[T <: ActiveRecordBase[_]]
      (implicit m: Manifest[T]): HasManyAssociation[this.type, T] =
        hasMany[T]().asInstanceOf[HasManyAssociation[this.type, T]]

    protected def hasMany[T <: ActiveRecordBase[_]]
      (conditions: Map[String, Any] = Map.empty, foreignKey: String = null)
      (implicit m: Manifest[T]): HasManyAssociation[this.type, T] = {
        val key = Option(foreignKey).getOrElse(
          Config.schema.foreignKeyFromClass(self.getClass))
        new HasManyAssociation[this.type, T](self, conditions, key)
      }

    protected def hasManyThrough[T <: ActiveRecordBase[_], I <: ActiveRecordBase[_]](
        through: HasManyAssociation[this.type, I],
        conditions: Map[String, Any] = Map.empty,
        foreignKey: String = null
      )(implicit m1: Manifest[T], m2: Manifest[I]): HasManyThroughAssociation[this.type, T, I] = {
        val key = Option(foreignKey).getOrElse(
          Config.schema.foreignKeyFromClass(m1.erasure))

        new HasManyThroughAssociation[this.type, T, I](self, through, conditions, key)(m1, m2)
      }
  }
}
