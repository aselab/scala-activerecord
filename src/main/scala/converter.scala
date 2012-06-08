package com.github.aselab.activerecord

import java.util.{Date, UUID, TimeZone}
import java.sql.Timestamp
import org.scala_tools.time.Imports._
import org.joda.time.format.ISODateTimeFormat

trait Converter[T] {
  def serialize(v: Any): String = v.toString
  def deserialize(s: String): T
}

object StringConverter extends Converter[String] {
  def deserialize(s: String) = s
}

object IntegerConverter extends Converter[java.lang.Integer] {
  def deserialize(s: String) = s.toInt
}

object LongConverter extends Converter[java.lang.Long] {
  def deserialize(s: String) = s.toLong
}

object DoubleConverter extends Converter[java.lang.Double] {
  def deserialize(s: String) = s.toDouble
}

object BooleanConverter extends Converter[java.lang.Boolean] {
  def deserialize(s: String) = s.toBoolean
}

object BigDecimalConverter extends Converter[BigDecimal] {
  def deserialize(s: String) = BigDecimal(s)
}

object FloatConverter extends Converter[java.lang.Float] {
  def deserialize(s: String) = s.toFloat
}

object TimestampConverter extends Converter[Timestamp] {
  override def serialize(v: Any) = {
    val timezone = DateTimeZone.forTimeZone(TimeZone.getDefault)
    new DateTime(v).withZone(timezone).toString(ISODateTimeFormat.dateTime)
  }

  def deserialize(s: String) =
    new Timestamp(ISODateTimeFormat.dateTime.parseDateTime(s).millis)
}

object UUIDConverter extends Converter[UUID] {
  def deserialize(s: String) = UUID.fromString(s)
}

object DateConverter extends Converter[Date] {
  override def serialize(v: Any) = {
    val timezone = DateTimeZone.forTimeZone(TimeZone.getDefault)
    new DateTime(v).withZone(timezone).toString(ISODateTimeFormat.dateTime)
  }

  def deserialize(s: String) =
    ISODateTimeFormat.dateTime.parseDateTime(s).toDate
}

object Converter {
  private val converters = collection.mutable.Map[Class[_], Converter[_]](
    classOf[String] -> StringConverter,
    classOf[java.lang.Integer] -> IntegerConverter,
    classOf[Long] -> LongConverter,
    classOf[java.lang.Long] -> LongConverter,
    classOf[Double] -> DoubleConverter,
    classOf[java.lang.Double] -> DoubleConverter,
    classOf[Boolean] -> BooleanConverter,
    classOf[java.lang.Boolean] -> BooleanConverter,
    classOf[scala.math.BigDecimal] -> BigDecimalConverter,
    classOf[Float] -> FloatConverter,
    classOf[java.lang.Float] -> FloatConverter,
    classOf[Timestamp] -> TimestampConverter,
    classOf[UUID] -> UUIDConverter,
    classOf[Date] -> DateConverter
  )

  def register[T](fieldType: Class[T], converter: Converter[T]) = converters += (fieldType -> converter)
  def unregister(fieldType: Class[_]) = converters -= fieldType

  def get(fieldType: Class[_]) = converters.get(fieldType)
}

