package com.github.aselab.activerecord.io

import com.github.aselab.activerecord._
import validations._
import inner._
import reflections._

trait IO extends Validatable { this: ProductModel =>
  import ReflectionUtil._

  def toMap: Map[String, Any] = _companion.fields.flatMap { f =>
    this.getOption[Any](f.name).map(f.name -> _)
  }.toMap

  def toFormValues: Map[String, String] = toFormValues(None)

  def toFormValues(prefix: Option[String]): Map[String, String] = {
    def serialize(c: Class[_], value: Any, key: String): Map[String, String] =
      if (classOf[IO].isAssignableFrom(c)) {
        value.asInstanceOf[IO].toFormValues(Some(key))
      } else {
        Map(FormConverter.get(c).map(key -> _.serialize(value)).getOrElse(
          throw ActiveRecordException.unsupportedType(key)
        ))
      }

    toMap.flatMap { case (k, v) =>
      val info = _companion.fieldInfo(k)
      val key = prefix.map("%s[%s]".format(_, k)).getOrElse(k)

      if (info.isSeq) {
        v.asInstanceOf[Seq[_]].zipWithIndex.flatMap { case (value, index) =>
          val k = "%s[%d]".format(key, index)
          serialize(info.fieldType, value, k)
        }
      } else {
        serialize(info.fieldType, v, key)
      }
    }
  }

  def assign(data: Map[String, Any]): Unit = {
    data.foreach{ case (k, v) =>
      val info = _companion.fieldInfo(k)
      val value = if (info.isOption) Some(v) else v
      this.setValue(k, value)
    }
  }

  def assignFormValues(data: Map[String, String]): Unit = {
    assign(_companion.fieldInfo.flatMap {
      case (name, info) =>
        def converter = FormConverter.get(info.fieldType).getOrElse(
          throw ActiveRecordException.unsupportedType(name)
        )

        def deserialize(data: Map[String, String], key: String) = if (info.isModel) {
          val companion = classToCompanion(info.fieldType).asInstanceOf[FormSupport[IO]]
          val map = data.collect {
            case (k, v) if k.startsWith(key + "[") => (k.replaceFirst(key + """\[(\w+)\]""", "$1"), v)
          }
          if (!(info.isOption && map.nonEmpty)) Some(companion.bind(map, false)) else None
        } else {
          data.get(key).collect {
            case v if !(info.isOption && v.isEmpty) => converter.deserialize(v)
          }
        }

        try {
          if (info.isSeq) {
            val dataList = Stream.from(0).map(i => data.collect {
              case (k, v) if k.startsWith("%s[%d]".format(name, i)) =>
                (k.replaceFirst("""%s\[(%d)\]""".format(name, i), "$1"), v)
            }.toMap).takeWhile(_.nonEmpty)
            Some(name -> dataList.zipWithIndex.flatMap {
              case (d, i) => deserialize(d, i.toString)
            }.toList)
          } else {
            deserialize(data, name).map(name -> _)
          }
        } catch {
          case e: Throwable =>
            this.errors.add(name, Validator.ERROR_PREFIX + "invalid")
            None
        }
    })
  }
}

trait FormSupport[T <: ProductModel with IO] { self: ProductModelCompanion[T] =>
  import ReflectionUtil._

  def bind(data: Map[String, String], validate: Boolean = true)(implicit source: T = self.newInstance): T = {
    source.clearErrors
    source.assignFormValues(data)
    if (validate) source.validate(clear = false)
    source
  }

  def unbind(m: T): Map[String, String] = m.toFormValues
}
