package com.github.aselab.activerecord.experimental

import com.github.aselab.activerecord._

trait Versionable extends ActiveRecord with Serializable {
  import ReflectionUtil._

  @dsl.Ignore private lazy val _className = getClass.getName

  abstract override def doUpdate: Boolean = dsl.inTransaction {
    changed.foreach { case (name, value) =>
      Version(_className, this.id, name, value._1.toString, value._2.toString).save
    }
    changed.clear
    super.doUpdate
  }

  @dsl.Ignore private var changed = collection.mutable.Map[String, (Any, Any)]()

  private def setId(id: Long) = {
    val f = classOf[ActiveRecord].getDeclaredField("id")
    f.setAccessible(true)
    f.set(this, id)
  }

  def map(newValues: (String, Any)*): this.type = {
    val constructor = this.getClass.getConstructors.head
    val n = constructor.newInstance(productIterator.map(_.asInstanceOf[AnyRef]).toSeq:_*).asInstanceOf[this.type]
    (_companion.fields.map {
      f => (f.name, this.getValue[Any](f.name))
    }.toMap ++ newValues).foreach {
      case (k, v) => n.setValue(k, v)
    }
    n.setId(id)
    newValues.foreach {
      case (name, newValue) =>
        val oldValue = this.getValue[Any](name)
        if (oldValue != newValue) n.changed += ((name, (oldValue, newValue)))
    }
    n
  }

  def apply(newValues: (String, Any)*): this.type = map(newValues:_*)
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
