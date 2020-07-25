package com.github.aselab.activerecord.io

import com.github.aselab.activerecord._
import inner._
import reflections._
import scala.reflect.ClassTag
import scala.language.implicitConversions

import org.json4s._
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization

object JsonSerializer {
  def assignFunc(value: Any, fieldInfo: FieldInfo): Any = fieldInfo match {
    case f if f.isSeq && classOf[FormSerializer].isAssignableFrom(fieldInfo.fieldType) => FormSerializer.formAssignFunc(value, fieldInfo)(this.assignFunc)
    case f if f.isSeq => value.asInstanceOf[List[_]].map(v => deserializeValue(v, f))
    case f if f.isOption => value.asInstanceOf[Option[_]].map(v => deserializeValue(v, f))
    case _ => deserializeValue(value, fieldInfo)
  }

  def deserializeValue(value: Any, fieldInfo: FieldInfo): Any =
    FormConverter.get(fieldInfo.fieldType).map(_.deserialize(value.toString)).getOrElse(
      FormSerializer.formAssignFunc(value, fieldInfo)(this.assignFunc)
    )
}

trait JsonSerializer extends FormSerializer { self: ProductModel =>
  private[this] implicit def _format = DefaultFormats

  def asJson: JValue = Extraction.decompose(toSerializedMap)

  def asJson(onlyFields: List[String]): JValue =
    Extraction.decompose(toSerializedMap(onlyFields))

  def asJson(onlyFields: String*): JValue = asJson(onlyFields.toList)

  def toJson: String = Serialization.write(toSerializedMap)

  def toJson(onlyFields: List[String]): String =
    Serialization.write(toSerializedMap(onlyFields))

  def toJson(onlyFields: String*): String = toJson(onlyFields.toList)

  def fromJson(json: String, throws: Boolean = false): this.type = fromJValue(JsonMethods.parse(json))

  def fromJValue(jvalue: JValue, throws: Boolean = false): this.type =
    unsafeAssign(jvalue.values.asInstanceOf[Map[String, Any]], JsonSerializer.assignFunc(_, _), throws)
}

trait JsonSupport[T <: ActiveModel] { self: FormSupport[T] =>
  def fromJson(json: String): T = fromJValue(JsonMethods.parse(json))

  def fromArrayJson(json: String): List[T] = fromJArray(JsonMethods.parse(json))

  def fromJValue(jvalue: JValue, throws: Boolean = false): T =
    unsafeAssign(jvalue.values.asInstanceOf[Map[String, Any]], JsonSerializer.assignFunc, throws)

  def fromJArray(jarray: JValue, throws: Boolean = false): List[T] =
    jarray.values.asInstanceOf[List[Map[String, Any]]].map(m => unsafeAssign(m, JsonSerializer.assignFunc, throws))
}

trait JsonImplicits { self: DSL =>

  class IterableJsonSerializer[T <: ActiveModel: ClassTag](list: Iterable[T]) {
    private implicit val format = DefaultFormats

    def asJson: JValue = Extraction.decompose(list.map(_.toSerializedMap))

    def asJson(onlyFields: List[String]): JValue = Extraction.decompose(list.map(_.toSerializedMap(onlyFields)))

    def asJson(onlyFields: String*): JValue = asJson(onlyFields.toList)

    def toJson: String = Serialization.write(list.map(_.toSerializedMap))

    def toJson(onlyFields: List[String]): String =
      Serialization.write(list.map(_.toSerializedMap(onlyFields)))

    def toJson(onlyFields: String*): String = toJson(onlyFields.toList)
  }

  implicit def toJsonable[T <: ActiveModel: ClassTag](list: Iterable[T]) =
    new IterableJsonSerializer(list)

  // for Scala 2.11+ only
  implicit def toJsonable[T <: ActiveModel: ClassTag, ILike](list: ILike)(implicit ev:  ILike => Iterable[T]) =
    new IterableJsonSerializer(list)

  implicit class GroupByJsonable[T <: ActiveModel: ClassTag](grouped: Map[_, List[T]]) {
    private implicit val format = DefaultFormats

    def toJson: String = Serialization.write(grouped.toMap.map{ case (k, v) => (k.toString, v.asJson) })

    def toJson(onlyFields: List[String]): String =
      Serialization.write(grouped.toMap.map{ case (k, v) => (k.toString, v.asJson(onlyFields)) })

    def toJson(onlyFields: String*): String = toJson(onlyFields.toList)
  }
}
