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
  def toJson: String = {
    implicit val format =  DefaultFormats
    Serialization.write(toMap)
  }

  def toJson(onlyFields: List[String]): String = {
    implicit val format =  DefaultFormats
    Serialization.write(toMap(onlyFields))
  }

  def toJson(onlyFields: String*): String = toJson(onlyFields.toList)

  def fromJson(json: String): self.type = {
    implicit val format =  DefaultFormats
    assignFormValues(JsonMethods.parse(json).values.asInstanceOf[Map[String, Any]].mapValues(_.toString))
  }
}

trait JsonSupport[T <: ActiveModel] { self: FormSupport[T] =>
  def fromJson(json: String) = {
    implicit val format =  DefaultFormats
    bind(JsonMethods.parse(json).values.asInstanceOf[Map[String, Any]].mapValues(_.toString))
  }
}

trait JsonImplicits { self: DSL =>
  implicit class IterableJsonSerializer[T <: AR : ClassTag](list: Iterable[T]) {
    private implicit val format =  DefaultFormats

    def toJson: String = Serialization.write(list.map(_.toMap))

    def toJson(onlyFields: List[String]): String =
      Serialization.write(list.map(_.toMap(onlyFields)))

    def toJson(onlyFields: String*): String = toJson(onlyFields.toList)
  }

  // for Scala 2.11+ only
  implicit class IterableLikeJsonSerializer[T <: AR: ClassTag, ILike <% Iterable[T]](list: ILike) {
    private implicit val format =  DefaultFormats

    def toJson: String = Serialization.write(list.map(_.toMap))

    def toJson(onlyFields: List[String]): String =
      Serialization.write(list.map(_.toMap(onlyFields)))

    def toJson(onlyFields: String*): String = toJson(onlyFields.toList)
  }
}
