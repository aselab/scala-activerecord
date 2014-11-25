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

  def toMap(onlyFields: List[String]): Map[String, Any] = onlyFields.flatMap { name =>
    this.getOption[Any](name).map(name -> _)
  }.toMap

  def toMap(onlyFields: String*): Map[String, Any] = toMap(onlyFields.toList)

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
      val key = prefix.map(FormUtil.join(_, k)).getOrElse(k)

      if (info.isSeq) {
        v.asInstanceOf[Seq[_]].zipWithIndex.flatMap { case (value, index) =>
          serialize(info.fieldType, value, FormUtil.join(key, index))
        }
      } else {
        serialize(info.fieldType, v, key)
      }
    }
  }

  def assign(data: Map[String, Any]): this.type = {
    data.foreach{ case (k, v) =>
      val info = _companion.fieldInfo(k)
      val value = if(info.isOption) {
        v match {
          case s:Some[_] => s
          case "" | None => None
          case _ => Some(v)
        }
      } else v

      (value, _companion.fieldInfo(k)) match {
        case (v: BigInt, FieldInfo(name, klass, _, _, _)) if klass == classOf[Int] => this.setValue(k, v.toInt)
        case (v, FieldInfo(name, klass, _, isSeq, _)) if classOf[IO].isAssignableFrom(klass) =>
          val companion = classToCompanion(klass).asInstanceOf[FormSupport[ActiveModel]]
          try {
            this.setValue(k, if (isSeq) {
              v.asInstanceOf[List[Map[String, Any]]].map(companion.assign)
            } else {
              companion.assign(v.asInstanceOf[Map[String, Any]])
            })
          } catch {
            case e => this.setValue(k, value)
          }
        case _ => this.setValue(k, value)
      }
    }
    this
  }

  def assignFormValues(data: Map[String, String]): this.type = {
    assign(_companion.fieldInfo.flatMap {
      case (name, info) =>
        def converter = FormConverter.get(info.fieldType).getOrElse(
          throw ActiveRecordException.unsupportedType(name)
        )

        def deserialize(data: Map[String, String], key: String) = if (info.isModel) {
          val companion = classToCompanion(info.fieldType).asInstanceOf[FormSupport[ActiveModel]]
          val map = data.collect {
            case (k, v) if k.startsWith(key + "[") => FormUtil.shift(k) -> v
          }
          if (!(info.isOption && map.nonEmpty)) Some(companion.bind(map)) else None
        } else {
          data.get(key).collect {
            case v if !(info.isOption && v.isEmpty) => converter.deserialize(v)
            case _ => ""
          }
        }

        try {
          if (info.isSeq) {
            val dataList = Stream.from(0).map(i => data.collect {
              case (k, v) if k.startsWith("%s[%d]".format(name, i)) => FormUtil.shift(k) -> v
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

  def formErrors: Seq[ValidationError] = {
    val nestErrors = _companion.validatableFields.flatMap { f =>
      f.toSeq[IO](this).zipWithIndex.flatMap { case (m, i) =>
        m.formErrors.map {
          case e if e.isGlobal => e.copy(model = this.getClass, key = f.name + e.key)
          case e if f.isSeq => e.copy(key = FormUtil.join(f.name, i, e.key))
          case e => e.copy(key = FormUtil.join(f.name, e.key))
        }
      }
    }
    errors.toSeq ++ nestErrors
  }

  override def validate(): Boolean = super.validate && formErrors.isEmpty
}

object FormUtil {
  /** a[b][c] => b[c] */
  def shift(s: String): String = s.replaceFirst("""[^\[]+\[([^\[\]]+)\]""", "$1")

  /** a[b][c] => a, b, c */
  def split(s: String): Seq[String] = s.replaceAll("""\[([^\[\]]*)\]""", ",$1").split(",")

  /** a, b, c[d] => a[b][c][d] */
  def join(a: String, b: Any*): String =
    a + b.flatMap(s => split(s.toString)).map("[%s]".format(_)).mkString
}

trait FormSupport[T <: ActiveModel] { self: ProductModelCompanion[T] =>
  import ReflectionUtil._

  type C = ActiveModelCompanion[ActiveModel]

  def isRequired(name: String): Boolean = {
    def inner(c: C, names: Seq[String]): Boolean = {
      (names.headOption, names.tail) match {
        case (Some(name), tail) =>
          c.fieldInfo.get(name).map { info =>
            if (tail.isEmpty)
              info.isRequired
            else
              inner(classToCompanion(info.fieldType).asInstanceOf[C], tail)
          }.getOrElse(false)
        case _ => false
      }
    }
    inner(this.asInstanceOf[C], FormUtil.split(name).filterNot(s => s.isEmpty || s.matches("^[0-9]+$")))
  }

  def bind(data: Map[String, String])(implicit source: T = self.newInstance): T = {
    source.assignFormValues(data)
    source
  }

  def assign(data: Map[String, Any])(implicit source: T = self.newInstance): T = {
    source.assign(data)
    source
  }

  def unbind(m: T): Map[String, String] = m.toFormValues
}
