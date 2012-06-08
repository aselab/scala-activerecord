package com.github.aselab.activerecord

import java.util.{Date, UUID, TimeZone}
import java.sql.Timestamp
import org.scala_tools.time.Imports._
import org.joda.time.format.ISODateTimeFormat

trait Converter[A, B] {
  def serialize(v: Any): B
  def deserialize(s: B): A
}

trait FormConverter[T] extends Converter[T, String] {
  override def serialize(v: Any): String = v.toString
}

object FormConverter {
  private[activerecord] lazy val converters = collection.mutable.Map[Class[_], FormConverter[_]](
    classOf[String] -> stringConverter,
    classOf[java.lang.Integer] -> integerConverter,
    classOf[Long] -> longConverter,
    classOf[java.lang.Long] -> longConverter,
    classOf[Double] -> doubleConverter,
    classOf[java.lang.Double] -> doubleConverter,
    classOf[Boolean] -> booleanConverter,
    classOf[java.lang.Boolean] -> booleanConverter,
    classOf[scala.math.BigDecimal] -> bigDecimalConverter,
    classOf[Float] -> floatConverter,
    classOf[java.lang.Float] -> floatConverter,
    classOf[Timestamp] -> timestampConverter,
    classOf[UUID] -> uuidConverter,
    classOf[Date] -> dateConverter
  )

  val stringConverter = new FormConverter[String] {
    def deserialize(s: String) = s
  }

  val integerConverter = new FormConverter[java.lang.Integer] {
    def deserialize(s: String) = s.toInt
  }

  val longConverter = new FormConverter[java.lang.Long] {
    def deserialize(s: String) = s.toLong
  }

  val doubleConverter = new FormConverter[java.lang.Double] {
    def deserialize(s: String) = s.toDouble
  }

  val booleanConverter = new FormConverter[java.lang.Boolean] {
    def deserialize(s: String) = s.toBoolean
  }

  val bigDecimalConverter = new FormConverter[BigDecimal] {
    def deserialize(s: String) = BigDecimal(s)
  }

  val floatConverter = new FormConverter[java.lang.Float] {
    def deserialize(s: String) = s.toFloat
  }

  val timestampConverter = new FormConverter[Timestamp] {
    override def serialize(v: Any) = {
      val timezone = DateTimeZone.forTimeZone(TimeZone.getDefault)
      new DateTime(v).withZone(timezone).toString(ISODateTimeFormat.dateTime)
    }

    def deserialize(s: String) =
      new Timestamp(ISODateTimeFormat.dateTime.parseDateTime(s).millis)
  }

  val uuidConverter = new FormConverter[UUID] {
    def deserialize(s: String) = UUID.fromString(s)
  }

  val dateConverter = new FormConverter[Date] {
    override def serialize(v: Any) = {
      val timezone = DateTimeZone.forTimeZone(TimeZone.getDefault)
      new DateTime(v).withZone(timezone).toString(ISODateTimeFormat.dateTime)
    }

    def deserialize(s: String) =
      ISODateTimeFormat.dateTime.parseDateTime(s).toDate
  }

  def register[T](fieldType: Class[T], converter: FormConverter[T]) = converters += (fieldType -> converter)
  def unregister(fieldType: Class[_]) = converters -= fieldType

  def get(fieldType: Class[_]) = converters.get(fieldType)
}

