package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import org.squeryl.dsl.ast.{LogicalBoolean, EqualityExpression}
import squeryl.Implicits._
import ReflectionUtil._

trait Associations {
  trait Association[K1, K2, O <: ActiveRecordBase[K1], T <: ActiveRecordBase[K2]] {
    val owner: O
    val associationClass: Class[T]

    protected lazy val companion = classToCompanion(associationClass)
      .asInstanceOf[ActiveRecordBaseCompanion[K2, T]]

    protected lazy val associationSource =
      ActiveRecord.Relation(companion.table, companion, {m: T => m})
  }

  class BelongsToAssociation[K1, K2, O <: ActiveRecordBase[K1], T <: ActiveRecordBase[K2]](
    val owner: O, val associationClass: Class[T], foreignKey: String
  ) extends Association[K1, K2, O, T] {

    def this(owner: O, associationClass: Class[T]) = this(owner, associationClass,
      Config.schema.foreignKeyFromClass(associationClass))

    lazy val fieldInfo = owner._companion.fieldInfo(foreignKey)
    
    def condition: T => LogicalBoolean = {
      m => fieldInfo.toEqualityExpression(m.id, owner.getValue(foreignKey))
    }

    def relation = associationSource.where(condition).limit(1)

    def get: Option[T] = relation.headOption

    def assign(m: T): T = {
      val v = if (fieldInfo.isOption) {
        Option(m.id)
      } else {
        m.id
      }
      owner.setValue(foreignKey, v)
      m
    }

    def :=(m: T): T = assign(m)
  }

  class HasManyAssociation[K1, K2, O <: ActiveRecordBase[K1], T <: ActiveRecordBase[K2]](
    val owner: O, val associationClass: Class[T], foreignKey: String
  ) extends Association[K1, K2, O, T] {

    def this(owner: O, associationClass: Class[T]) = this(owner, associationClass,
      Config.schema.foreignKeyFromClass(owner.getClass))

    lazy val fieldInfo = companion.fieldInfo(foreignKey)

    def condition: T => LogicalBoolean = {
      m => fieldInfo.toEqualityExpression(m.getValue(foreignKey), owner.id)
    }

    def relation = associationSource.where(condition)

    def associate(m: T): T = inTransaction {
      val v = if (fieldInfo.isOption) {
        Option(owner.id)
      } else {
        owner.id
      }
      m.setValue(foreignKey, v)
      m.save
      m
    }

    def <<(m: T): T = associate(m)
  }

  class HasManyThroughAssociation[K1, K2, O <: ActiveRecordBase[K1], T <: ActiveRecordBase[K2], I <: ActiveRecordBase[_]](
    val owner: O, val associationClass: Class[T],
    val through: HasManyAssociation[K1, _, O, I]
  )(implicit m: Manifest[I]) extends Association[K1, K2, O, T] {
    protected lazy val throughCompanion = classToCompanion(m.erasure)
      .asInstanceOf[ActiveRecordBaseCompanion[_, I]]

    val foreignKey = Config.schema.foreignKeyFromClass(owner.getClass)
    val associationForeignKey = Config.schema.foreignKeyFromClass(associationClass)

    lazy val sourceFieldInfo = throughCompanion.fieldInfo(foreignKey)
    lazy val associationFieldInfo = throughCompanion.fieldInfo(associationForeignKey)

    def relation = associationSource.joins[I]{
      (m, inter) =>
        val e1 = associationFieldInfo.toExpression(m.id)
        val e2 = associationFieldInfo.toExpression(inter.getValue(associationForeignKey))
        new EqualityExpression(e1, e2)
    }.where(
      (m, inter) => through.condition(inter)
    )
  }
}
