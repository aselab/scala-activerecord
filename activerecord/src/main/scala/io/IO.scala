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

  def toSerializedMap = toSerialized(toMap)

  def toSerializedMap(onlyFields: List[String]) = toSerialized(toMap(onlyFields))

  protected def toSerialized(map: Map[String, Any]): Map[String, Any] = map.mapValues {
    case v: Seq[_] => v.map(serializedValue)
    case v => serializedValue(v)
  }

  protected def serializedValue(value: Any) =
    if (isPrimitiveNumeric(value.getClass) || isBoolean(value.getClass)) {
      value
    } else {
      FormConverter.get(value.getClass).map(_.serialize(value).toString).getOrElse(value)
    }

  import scala.language.experimental.macros
  def assign(data: (String, Any)*): this.type = macro MethodMacros.assign[this.type]

  def assign(data: Map[String, Any]): this.type = assign(data, (v: Any, f: FieldInfo) => v)

  def assign(data: Map[String, Any], assignFunc: (Any, FieldInfo) => Any): this.type = {
    val fieldInfo = _companion.fieldInfo
    data.foreach { case (k, v) =>
      val info = fieldInfo(k)
      val value = toFieldType(v, info)
      this.setValue(k, assignFunc(value, info))
    }
    this
  }

  protected def toFieldType(value: Any, fieldInfo: FieldInfo) =
    if (fieldInfo.isOption) { toOption(value) } else { value }
}
