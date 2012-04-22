package com.github.aselab.activerecord

import annotations._

trait Versionable extends ActiveRecord {
  @Ignore private lazy val _className = getClass.getName

  abstract override def doUpdate = dsl.transaction {
    changed.foreach { case (name, value) =>
      Version(_className, this.id, name, value._1.toString, value._2.toString).save
    }
    changed.clear
    super.doUpdate
  }

  @Ignore private[activerecord] var changed = collection.mutable.Map[String, (Any, Any)]()

  abstract override def map(newValues: (String, Any)*) = {
    import ReflectionUtil._
    val n = super.map(newValues:_*).asInstanceOf[Versionable]
    newValues.foreach {
      case (name, newValue) =>
        val oldValue = this.getValue[Any](name)
        if (oldValue != newValue)
          n.changed += ((name, (oldValue, newValue)))
    }
    n
  }
}

case class Version(
  targetTable: String,
  targetId: Long,
  field: String,
  oldValue: String,
  newValue: String
) extends ActiveRecord

object Version extends ActiveRecordCompanion[Version]

trait VersionTable extends org.squeryl.Schema {
  val _versionTable = table[Version]
}
