package com.github.aselab.activerecord.io

import com.github.aselab.activerecord._
import inner._
import validations._
import reflections._

object FormSerializer {
  import ReflectionUtil._

  def assignFunc(value: Any, fieldInfo: FieldInfo): Any =
    formAssignFunc(value, fieldInfo)((v: Any, k: FieldInfo) => v)

  def formAssignFunc(value: Any, fieldInfo: FieldInfo)(assignFunc: (Any, FieldInfo) => Any): Any = {
    (value, fieldInfo) match {
      case (v: FormSerializer, FieldInfo(name, klass, _, isSeq, _)) if classOf[FormSerializer].isAssignableFrom(klass) && !isSeq =>
        value
      case (v, FieldInfo(name, klass, _, isSeq, _)) if classOf[FormSerializer].isAssignableFrom(klass) && !isSeq =>
        val companion = classToCompanion(klass).asInstanceOf[FormSupport[ActiveModel]]
        companion.unsafeAssign(v.asInstanceOf[Map[String, Any]], assignFunc)
      case (v, FieldInfo(name, klass, _, isSeq, _)) if classOf[FormSerializer].isAssignableFrom(klass) && isSeq =>
        if (v.asInstanceOf[List[_]].headOption.exists(e => classOf[FormSerializer].isAssignableFrom(e.getClass))) {
          value
        } else {
          val companion = classToCompanion(klass).asInstanceOf[FormSupport[ActiveModel]]
          v.asInstanceOf[List[Map[String, Any]]].map(companion.unsafeAssign(_, assignFunc))
        }
      case _ => value
    }
  }
}

trait FormSerializer extends IO { self: ProductModel =>
  import ReflectionUtil._

  override def toFieldType(value: Any, fieldInfo: FieldInfo): Any = {
    if (fieldInfo.isOption) {
      value match {
        case Some(v) => Some(v)
        case null | None | "" => None
        case v => Some(v)
      }
    } else { value }
  }

  def toFormValues: Map[String, String] = toFormValues(None)

  def toFormValues(prefix: Option[String]): Map[String, String] = {
    def serialize(c: Class[_], value: Any, key: String): Map[String, String] =
      if (classOf[FormSerializer].isAssignableFrom(c)) {
        value.asInstanceOf[FormSerializer].toFormValues(Some(key))
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

  def assignFormValues(data: Map[String, String]): this.type = {
    unsafeAssign(_companion.fieldInfo.flatMap {
      case (name, info) =>
        lazy val converter = FormConverter.get(info.fieldType).getOrElse(
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
            val dataList = LazyList.from(0).map(i => data.collect {
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
    }, FormSerializer.assignFunc(_, _))
  }

  def formErrors: Seq[ValidationError] = {
    val nestErrors = _companion.validatableFields.flatMap { f =>
      f.toSeq[FormSerializer](this).zipWithIndex.flatMap { case (m, i) =>
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

trait FormSupport[T <: ActiveModel] { self: ProductModelCompanion[T] =>
  import ReflectionUtil._

  type C = ActiveModelCompanion[ActiveModel]

  def isRequired(name: String): Boolean = {
    def inner(c: C, names: Array[String]): Boolean = {
      (names.headOption, names.tail) match {
        case (Some(name), tail) =>
          c.fieldInfo.get(name).map { info =>
            if (tail.isEmpty) {
              info.isRequired
            } else {
              inner(classToCompanion(info.fieldType).asInstanceOf[C], tail)
            }
          }.getOrElse(false)
        case _ => false
      }
    }
    inner(this.asInstanceOf[C], FormUtil.split(name).filterNot(s => s.isEmpty || s.matches("^[0-9]+$")))
  }

  def assignValue(value: Any, fieldInfo: FieldInfo): Any = {
    (value, fieldInfo) match {
      case (v: FormSerializer, FieldInfo(name, klass, _, isSeq, _)) if classOf[FormSerializer].isAssignableFrom(klass) && !isSeq =>
        value
      case (v, FieldInfo(name, klass, _, isSeq, _)) if classOf[FormSerializer].isAssignableFrom(klass) && !isSeq =>
        val companion = classToCompanion(klass).asInstanceOf[FormSupport[ActiveModel]]
        companion.unsafeAssign(v.asInstanceOf[Map[String, Any]], (v, k) => v)
      case (v, FieldInfo(name, klass, _, isSeq, _)) if classOf[FormSerializer].isAssignableFrom(klass) && isSeq =>
        if (v.asInstanceOf[List[_]].headOption.exists(e => classOf[FormSerializer].isAssignableFrom(e.getClass))) {
          value
        } else {
          val companion = classToCompanion(klass).asInstanceOf[FormSupport[ActiveModel]]
          v.asInstanceOf[List[Map[String, Any]]].map(companion.unsafeAssign(_, (v, k) => v))
        }
      case _ => value
    }
  }

  def bind(data: Map[String, String])(implicit source: T = self.newInstance): T = {
    source.assignFormValues(data)
    source
  }

  def unsafeAssign(data: Map[String, Any], assignFunc: (Any, FieldInfo) => Any, throws: Boolean = true)(implicit source: T = self.newInstance): T = {
    source.unsafeAssign(data, assignFunc, throws)
    source
  }

  def unbind(m: T): Map[String, String] = m.toFormValues
}

object FormUtil {
  /** a[b][c] => b[c] */
  def shift(s: String): String = s.replaceFirst("""[^\[]+\[([^\[\]]+)\]""", "$1")

  /** a[b][c] => a, b, c */
  def split(s: String): Array[String] = s.replaceAll("""\[([^\[\]]*)\]""", ",$1").split(",")

  /** a, b, c[d] => a[b][c][d] */
  def join(a: String, b: Any*): String =
    a + b.flatMap(s => split(s.toString)).map("[%s]".format(_)).mkString
}
