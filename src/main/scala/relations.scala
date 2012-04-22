package com.github.aselab.activerecord

import org.squeryl.{KeyedEntity, Session}
import org.squeryl.dsl._

trait RecordRelation

case class ActiveRecordOneToMany[M](override val relation: OneToMany[M]) extends StatefulOneToMany(relation) with RecordRelation
{
  
  override def refresh = dsl.inTransaction {
    super.refresh
  }

  override def associate(m: M)(implicit ev: M <:< KeyedEntity[_]) =
    dsl.inTransaction { super.associate(m) }

  override def deleteAll = dsl.inTransaction { super.deleteAll }
}

case class ActiveRecordManyToOne[O <: ActiveRecord](override val relation: ManyToOne[O]) extends StatefulManyToOne(relation) with RecordRelation
{
  
  override def refresh = dsl.inTransaction {
    super.refresh
  }

  override def assign(o: O) = dsl.inTransaction { super.assign(o) }

  override def delete = dsl.inTransaction { super.delete }
}

case class RelationWrapper[L <: ActiveRecord, R <: ActiveRecord](relation: Relation[L, R]) {
  def oneToManyRelation = relation.asInstanceOf[OneToManyRelation[L, R]]
  def belongsTo(m: R) = ActiveRecordManyToOne(oneToManyRelation.right(m))
  def hasMany(m: L) = ActiveRecordOneToMany(oneToManyRelation.left(m))
}
