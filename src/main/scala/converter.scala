package com.github.aselab.activerecord

import java.util.{Date, UUID}
import java.sql.Timestamp
import org.scala_tools.time.Imports._
import org.joda.time.format.ISODateTimeFormat

trait Converter[T] {
  def serialize(s: String): T
  def deserialize(v: Any): String = v.toString
}

object StringConverter extends Converter[String] {
  def serialize(s: String) = s
}

object IntegerConverter extends Converter[java.lang.Integer] {
  def serialize(s: String) = s.toInt
}

object LongConverter extends Converter[java.lang.Long] {
  def serialize(s: String) = s.toLong
}

object DoubleConverter extends Converter[java.lang.Double] {
  def serialize(s: String) = s.toDouble
}

object BooleanConverter extends Converter[java.lang.Boolean] {
  def serialize(s: String) = s.toBoolean
}

object BigDecimalConverter extends Converter[BigDecimal] {
  def serialize(s: String) = BigDecimal(s)
}

object FloatConverter extends Converter[java.lang.Float] {
  def serialize(s: String) = s.toFloat
}

object TimestampConverter extends Converter[Timestamp] {
  def serialize(s: String) =
    new Timestamp(ISODateTimeFormat.dateTime.parseDateTime(s).millis)
  override def deserialize(v: Any) =
    new DateTime(v).toString(ISODateTimeFormat.dateTime)
}

object UUIDConverter extends Converter[UUID] {
  def serialize(s: String) = UUID.fromString(s)
}

object DateConverter extends Converter[Date] {
  def serialize(s: String) =
    ISODateTimeFormat.dateTime.parseDateTime(s).toDate
  override def deserialize(v: Any) =
    new DateTime(v).toString(ISODateTimeFormat.dateTime)
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

