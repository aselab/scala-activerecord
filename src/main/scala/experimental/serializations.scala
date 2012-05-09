package com.github.aselab.activerecord

trait IO { this: ActiveRecordBase[_] =>
  import ReflectionUtil._

  def toMap: Map[String, Any] = {
    _companion.formatFields.flatMap { f =>
      val name = f.getName
      (this.getValue[Any](name) match {
        case v: Option[_] => v
        case v => Some(v)
      }).map(name -> _)
    }.toMap
  }

  def assign(data: Map[String, Any]) = {
    import ReflectionUtil._
    data.foreach{case (k, v) =>
      //val info = _companion.fieldInfo(k)
      //val value = if (info.isOption) Some(v) else v
      val value = this.getValue[Any](k) match {
        case _: Option[_] => Some(v)
        case _ => v
      }
      this.setValue(k, value)
    }
  }
}
