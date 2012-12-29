package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import org.squeryl.dsl.ast.LogicalBoolean
import squeryl.Implicits._
import ReflectionUtil._

trait Associations {
  trait Association[K1, K2, O <: ActiveRecordBase[K1], T <: ActiveRecordBase[K2]] {
    protected var cached = false
    protected var cache: List[T] = Nil

    val owner: O
    val associationClass: Class[T]
    def condition: T => LogicalBoolean

    protected lazy val companion = classToCompanion(associationClass)
      .asInstanceOf[ActiveRecordBaseCompanion[K2, T]]

    def relation: ActiveRecord.Relation[K2, T, T] = {
      ActiveRecord.Relation(companion.table, companion, {m: T => m})
        .where(condition)
    }

    def toList: List[T] = if (cached) cache else reload.toList

    def reload: this.type = {
      inTransaction { cache = relation.toQuery.toList }
      cached = true
      this
    }
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

    def get: Option[T] = toList.headOption

    def assign(m: T): T = {
      cache = List(m)
      cached = true

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

    def associate(m: T): T = inTransaction {
      if (cached) reload
      cache :+= m

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
}
