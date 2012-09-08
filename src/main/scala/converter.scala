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
  override def serialize(v: Any): String = Option(v).map(_.toString).orNull
}

object FormConverter extends PrimitiveHandler[FormConverter[_]] {
  val stringHandler = new FormConverter[String] {
    def deserialize(s: String) = s
  }

  val intHandler = new FormConverter[java.lang.Integer] {
    def deserialize(s: String) = s.toInt
  }

  val longHandler = new FormConverter[java.lang.Long] {
    def deserialize(s: String) = s.toLong
  }

  val doubleHandler = new FormConverter[java.lang.Double] {
    def deserialize(s: String) = s.toDouble
  }

  val booleanHandler = new FormConverter[java.lang.Boolean] {
    def deserialize(s: String) = s.toBoolean
  }

  val bigDecimalHandler = new FormConverter[BigDecimal] {
    def deserialize(s: String) = BigDecimal(s)
  }

  val floatHandler = new FormConverter[java.lang.Float] {
    def deserialize(s: String) = s.toFloat
  }

  val timestampHandler = new FormConverter[Timestamp] {
    override def serialize(v: Any) = {
      val timezone = DateTimeZone.forTimeZone(TimeZone.getDefault)
      new DateTime(v).withZone(timezone).toString(ISODateTimeFormat.dateTime)
    }

    def deserialize(s: String) =
      new Timestamp(ISODateTimeFormat.dateTime.parseDateTime(s).millis)
  }

  val uuidHandler = new FormConverter[UUID] {
    def deserialize(s: String) = UUID.fromString(s)
  }

  val dateHandler = new FormConverter[Date] {
    override def serialize(v: Any) = {
      val timezone = DateTimeZone.forTimeZone(TimeZone.getDefault)
      new DateTime(v).withZone(timezone).toString(ISODateTimeFormat.dateTime)
    }

    def deserialize(s: String) =
      ISODateTimeFormat.dateTime.parseDateTime(s).toDate
  }
}

