package com.github.aselab.activerecord

import scala.reflect.classTag
import com.github.aselab.activerecord.dsl._

/**
 * Single Table Inheritance
 */
trait STI { self: ActiveRecordBase[_] =>
  @Column("type") val _type = this.getClass.getSimpleName
}

trait STISupport[T <: ActiveRecord with STI] { self: ActiveRecordBaseCompanion[Long, T] =>
  private[this] lazy val _typeName = classTag[T].runtimeClass.getSimpleName

  override def defaultScope = queryToRelation[T](table).where(_._type === _typeName)

  override def find(id: Long): Option[T] = defaultScope.find(id)

  override protected[activerecord] def delete(id: Long): Boolean =
    all.where(_.id === id).forceDestroyAll() > 0

  override def forceDeleteAll(): Int = all.forceDestroyAll()

  override def forceDelete(condition: T => LogicalBoolean): Int =
    all.where(condition).forceDestroyAll()
}
