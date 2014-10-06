package com.github.aselab.activerecord.io

import com.github.aselab.activerecord._
import ActiveRecord._
import aliases.AR
import inner._
import scala.reflect.ClassTag

import org.json4s._
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization

trait JsonSerializer { self: IO =>
  def asJson: JValue = {
    implicit val format =  DefaultFormats
    Extraction.decompose(toMap)
  }

  def asJson(onlyFields: List[String]): JValue = {
    implicit val format =  DefaultFormats
    Extraction.decompose(toMap(onlyFields))
  }

  def asJson(onlyFields: String*): JValue = asJson(onlyFields.toList)

  def toJson: String = {
    implicit val format =  DefaultFormats
    Serialization.write(toMap)
  }

  def toJson(onlyFields: List[String]): String = {
    implicit val format =  DefaultFormats
    Serialization.write(toMap(onlyFields))
  }

  def toJson(onlyFields: String*): String = toJson(onlyFields.toList)

  def fromJson(json: String): self.type = fromJValue(JsonMethods.parse(json))

  def fromJValue(jvalue: JValue): self.type =
    assignFormValues(jvalue.values.asInstanceOf[Map[String, Any]].mapValues(_.toString))
}

trait JsonSupport[T <: ActiveModel] { self: FormSupport[T] =>
  def fromJson(json: String): T = fromJValue(JsonMethods.parse(json))

  def fromArrayJson(json: String): List[T] = fromJArray(JsonMethods.parse(json))

  def fromJValue(jvalue: JValue): T =
    bind(jvalue.values.asInstanceOf[Map[String, Any]].mapValues(_.toString))

  def fromJArray(jarray: JValue): List[T] =
    jarray.values.asInstanceOf[List[Map[String, Any]]].map(m => bind(m.mapValues(_.toString)))
}

trait JsonImplicits { self: DSL =>

  class IterableJsonSerializer[T <: AR : ClassTag](list: Iterable[T]) {
    private implicit val format =  DefaultFormats

    def asJson: JValue = Extraction.decompose(list.map(_.toMap))

    def asJson(onlyFields: List[String]): JValue = Extraction.decompose(list.map(_.toMap(onlyFields)))

    def asJson(onlyFields: String*): JValue = asJson(onlyFields.toList)

    def toJson: String = Serialization.write(list.map(_.toMap))

    def toJson(onlyFields: List[String]): String =
      Serialization.write(list.map(_.toMap(onlyFields)))

    def toJson(onlyFields: String*): String = toJson(onlyFields.toList)
  }

  implicit def toJsonable[T <: AR : ClassTag](list: Iterable[T]) =
    new IterableJsonSerializer(list)

  // for Scala 2.11+ only
  implicit def toJsonable[T <: AR: ClassTag, ILike <% Iterable[T]](list: ILike) =
    new IterableJsonSerializer(list)

  implicit class GroupByJsonable[T <: AR: ClassTag](grouped: Map[_, List[T]]) {
    private implicit val format =  DefaultFormats

    def toJson: String = Serialization.write(grouped.map{ case (k, v) => (k.toString, v.asJson) })

    def toJson(onlyFields: List[String]): String =
      Serialization.write(grouped.map{ case (k, v) => (k.toString, v.asJson(onlyFields)) })

    def toJson(onlyFields: String*): String = toJson(onlyFields.toList)
  }
}
