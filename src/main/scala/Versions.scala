package com.github.aselab.activerecord

import annotations._

trait Versions extends ActiveRecordBase {
  @Ignore private lazy val _className = getClass.getSimpleName

  abstract override def doUpdate = dsl.transaction {
    changed.foreach { case (name, value) =>
      VersionRecord(_className, this.id, name, value._1.toString, value._2.toString).save
    }
    changed.clear
    super.doUpdate
  }

  @Ignore private[activerecord] var changed = collection.mutable.Map[String, (Any, Any)]()

  abstract override def map(newValues: (String, Any)*) = {
    import ReflectionUtil._
    val n = super.map(newValues:_*).asInstanceOf[Versions]
    newValues.foreach {
      case (name, newValue) =>
        val oldValue = this.getValue[Any](name)
        if (oldValue != newValue)
          n.changed += ((name, (oldValue, newValue)))
    }
    n
  }
}

case class VersionRecord(
  targetTable: String,
  targetId: Long,
  field: String,
  oldValue: String,
  newValue: String
) extends ActiveRecord

object VersionRecord extends ActiveRecordCompanion[VersionRecord]

