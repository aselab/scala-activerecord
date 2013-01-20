package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import dsl.keyedEntityDef

/**
 * Base class of intermediate table for many to many relationship.
 */
abstract class IntermediateRecord extends ActiveRecordBase[CompositeKey2[Long, Long]] with KeyedEntity[CompositeKey2[Long, Long]]

/**
 * Base class of IntermediateRecord companion objects.
 */
trait IntermediateRecordCompanion[T <: IntermediateRecord]
  extends ActiveRecordBaseCompanion[CompositeKey2[Long, Long], T]

case class DefaultIntermediateRecord() extends IntermediateRecord {
  val leftId: Long = 0
  val rightId: Long = 0
  def id: CompositeKey2[Long, Long] = compositeKey(leftId, rightId)
}

object DefaultIntermediateRecord {
  type K = CompositeKey2[Long, Long]
  val keyedEntityDef = new KeyedEntityDef[DefaultIntermediateRecord, K] {
    def getId(m: DefaultIntermediateRecord) = m.id
    def isPersisted(m: DefaultIntermediateRecord) = m.isPersisted
    def idPropertyName = "id"
  }
}
