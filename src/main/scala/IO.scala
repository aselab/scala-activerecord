package com.github.aselab.activerecord

trait IO { this: ProductModel =>
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

  def toFormValues: Map[String, String] =
    toMap.flatMap { case (k, v) =>
      val info = _companion.fieldInfo(k)
      val fieldType = info.fieldType
      if (info.isSeq) {
        v.asInstanceOf[List[_]].zipWithIndex.map{ case (value, index) =>
          val key = "%s[%d]".format(k, index)
          val converter = FormConverter.get(value.getClass).getOrElse(ActiveRecordException.unsupportedType(key))
          key -> converter.serialize(value)
        }
      } else {
        val converter = FormConverter.get(info.fieldType).getOrElse(ActiveRecordException.unsupportedType(k))
        Option(k -> converter.serialize(v))
      }
    }

  def assign(data: Map[String, Any]) = {
    import ReflectionUtil._
    data.foreach{ case (k, v) =>
      val info = _companion.fieldInfo(k)
      val value = if (info.isOption) Some(v) else v
      this.setValue(k, value)
    }
  }

  def assignFormValues(data: Map[String, String]) = {
    assign(_companion.fieldInfo.flatMap {
      case (name, info) =>
        val converter = FormConverter.get(info.fieldType).getOrElse(ActiveRecordException.unsupportedType(name))
        val v = data.get(name)
        try {
          if (info.isSeq) {
            val keys = Stream.from(0).map("%s[%d]".format(name, _)).takeWhile(data.isDefinedAt)
            Option(name -> keys.map(key => converter.deserialize(data(key))).toList)
          } else if (info.isOption && v.get.isEmpty) {
            None
          } else if (info.required && v.get.isEmpty) {
            this.errors.add(name, "is required")
            None
          } else {
            Option(name -> converter.deserialize(v.get))
          }
        } catch {
          case e =>
            this.errors.add(name, "is invalid")
            None
        }
    })
  }
}

trait FormSupport[T <: ProductModel] {self: ProductModelCompanion[T] =>
  import ReflectionUtil._

  def bind(data: Map[String, String])(implicit source: T = self.newInstance): T = {
    source.assignFormValues(data)
    source
  }

  def unbind(m: T): Map[String, String] = m.toFormValues
}
