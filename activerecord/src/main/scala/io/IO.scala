package com.github.aselab.activerecord.io

import com.github.aselab.activerecord._
import validations._
import inner._
import reflections._
import scala.util.control.NonFatal

trait IO extends Validatable { this: ProductModel =>
  import ReflectionUtil._

  def toMap: Map[String, Any] = _companion.fields.flatMap { f =>
    this.getOption[Any](f.name).map(f.name -> _)
  }.toMap

  def toMap(onlyFields: List[String]): Map[String, Any] = onlyFields.flatMap { name =>
    this.getOption[Any](name).map(name -> _)
  }.toMap

  def toMap(onlyFields: String*): Map[String, Any] = toMap(onlyFields.toList)

  def toSerializedMap: Map[String, Any] = toSerialized(toMap)

  def toSerializedMap(onlyFields: List[String]): Map[String, Any] = toSerialized(toMap(onlyFields))

  protected def toSerialized(map: Map[String, Any]): Map[String, Any] = map.view.mapValues {
    case v: Seq[_] => v.map(serializedValue)
    case v => serializedValue(v)
  }.toMap

  protected def serializedValue(value: Any): Any = value match {
    // nested models become maps here, so that json4s never has to reflect on model classes
    // (its case class reflection needs scala3-staging at run time on Scala 3)
    case m: IO => m.toSerializedMap
    case _ if isPrimitiveNumeric(value.getClass) || isBoolean(value.getClass) || isMap(value.getClass) => value
    case _ => FormConverter.get(value.getClass).map(_.serialize(value).toString).getOrElse(value)
  }

  /** Assigns field values; the names and value types are checked at compile time. */
  inline def assign(inline data: (String, Any)*): this.type = ${ MethodMacros.assign[this.type]('this, 'data) }

  /** Assigns the field values of a `Map(...)` literal; the names and value types are checked at compile time. */
  inline def assign(inline data: Map[String, Any]): this.type = ${ MethodMacros.assignMap[this.type]('this, 'data) }

  def unsafeAssign(data: Map[String, Any]): this.type = unsafeAssign(data, (v: Any, f: FieldInfo) => v)

  def unsafeAssign(data: Map[String, Any], assignFunc: (Any, FieldInfo) => Any, throws: Boolean = true): this.type = {
    val fieldInfo = _companion.fieldInfo
    data.foreach { case (k, v) =>
      try {
        val info = fieldInfo(k)
        val value = toFieldType(v, info)
        this.setValue(k, assignFunc(value, info))
      } catch {
        case NonFatal(e) => if (throws) { throw e }
      }
    }
    this
  }

  protected def toFieldType(value: Any, fieldInfo: FieldInfo) =
    if (fieldInfo.isOption) { toOption(value) } else { value }
}
