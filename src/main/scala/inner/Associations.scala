package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import org.squeryl.dsl.ast.{LogicalBoolean, EqualityExpression}
import squeryl.Implicits._
import ReflectionUtil._

trait Associations {
  trait Association[O <: ActiveRecordBase[_], T <: ActiveRecordBase[_]] {
    val owner: O
    val manifest: Manifest[T]
    val associationClass = manifest.erasure

    protected lazy val companion = classToCompanion(associationClass)
      .asInstanceOf[ActiveRecordBaseCompanion[_, T]]

    protected lazy val associationSource =
      ActiveRecord.Relation(companion.table, {m: T => m})(manifest)

    protected def fieldInfo(name: String) = companion.fieldInfo.getOrElse(name,
      throw ActiveRecordException.notFoundField(name)) 
  }

  class BelongsToAssociation[O <: ActiveRecordBase[_], T <: ActiveRecordBase[_]](
    val owner: O, foreignKey: String
  )(implicit val manifest: Manifest[T]) extends Association[O, T] {

    def this(owner: O)(implicit m: Manifest[T]) = this(owner,
      Config.schema.foreignKeyFromClass(m.erasure))

    lazy val fieldInfo = owner._companion.fieldInfo(foreignKey)
    
    def condition: T => LogicalBoolean = {
      m => fieldInfo.toEqualityExpression(m.id, owner.getValue(foreignKey))
    }

    def relation = associationSource.where(condition).limit(1)

    def get: Option[T] = relation.headOption

    def assign(m: T): T = {
      fieldInfo.setValue(owner, m.id)
      m
    }

    def :=(m: T): T = assign(m)
  }

  class HasManyAssociation[O <: ActiveRecordBase[_], T <: ActiveRecordBase[_]](
    val owner: O, additionalConditions: Map[String, Any], foreignKey: String
  )(implicit val manifest: Manifest[T]) extends Association[O, T] {

    def this(owner: O, additionalConditions: Map[String, Any])
      (implicit m: Manifest[T]) = this(owner, additionalConditions,
        Config.schema.foreignKeyFromClass(owner.getClass))

    private def conditions = additionalConditions + (foreignKey -> owner.id)

    def condition: T => LogicalBoolean = {
      m => LogicalBoolean.and(conditions.map {
        case (key, value) =>
          fieldInfo(key).toEqualityExpression(m.getValue(key), value)
      }.toSeq)
    }

    def relation = associationSource.where(condition)

    def build: T = assign(companion.newInstance)

    def assign(m: T): T = {
      conditions.foreach {
        case (key, value) => fieldInfo(key).setValue(m, value)
      }
      m
    }

    def associate(m: T): Boolean = inTransaction {
      assign(m).save
    }

    def <<(m: T): Boolean = associate(m)
  }

  class HasManyThroughAssociation[O <: ActiveRecordBase[_], T <: ActiveRecordBase[_], I <: ActiveRecordBase[_]](
    val owner: O, val through: HasManyAssociation[O, I],
    additionalConditions: Map[String, Any]
  )(implicit val manifest: Manifest[T], m: Manifest[I]) extends Association[O, T] {
    protected lazy val throughCompanion = classToCompanion(m.erasure)
      .asInstanceOf[ActiveRecordBaseCompanion[_, I]]

    protected def throughFieldInfo(name: String) =
      throughCompanion.fieldInfo.getOrElse(name,
        throw ActiveRecordException.notFoundField(name)) 

    val foreignKey = Config.schema.foreignKeyFromClass(owner.getClass)
    val associationForeignKey = Config.schema.foreignKeyFromClass(associationClass)

    def relation = associationSource.joins[I]{
      (m, inter) =>
        val f = fieldInfo("id")
        val e1 = f.toExpression(m.id)
        val e2 = f.toExpression(inter.getValue(associationForeignKey))
        new EqualityExpression(e1, e2)
    }.where(
      (m, inter) =>
      LogicalBoolean.and(through.condition(inter) :: additionalConditions.map {
        case (key, value) =>
          fieldInfo(key).toEqualityExpression(m.getValue(key), value)
      }.toList)
    )

    def assign(m: T): I = {
      additionalConditions.foreach {
        case (key, value) => fieldInfo(key).setValue(m, value)
      }
      val inter = throughCompanion.newInstance
      throughFieldInfo(foreignKey).setValue(inter, owner.id)
      throughFieldInfo(associationForeignKey).setValue(inter, m.id)
      inter
    }

    def associate(m: T): Boolean = inTransaction {
      assign(m).save
    }

    def <<(m: T): Boolean = associate(m)
  }
}
