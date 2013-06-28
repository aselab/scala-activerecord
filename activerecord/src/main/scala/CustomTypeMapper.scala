package com.github.aselab.activerecord

import io._
import reflections._
import java.util.Date
import java.sql.Timestamp
import org.squeryl.dsl._

abstract class CustomTypeMapper[A, B](primitiveTypeMode: org.squeryl.PrimitiveTypeMode)(implicit m: Manifest[A]) {
  def fromJdbc(t: A): B
  def toJdbc(t: B): A
  def defaultValue: B
  def formConverter: FormConverter[B] = null

  class JdbcConverter[T](anyTEF: PrimitiveJdbcMapper[_]) extends NonPrimitiveJdbcMapper[A, B, T](
    anyTEF.asInstanceOf[PrimitiveJdbcMapper[A]], primitiveTypeMode
  ) {
    def convertFromJdbc(t: A) = fromJdbc(t)
    def convertToJdbc(t: B) = toJdbc(t)
  }

  class OptionConverter[T, MT, OMT] extends TypedExpressionFactory[Option[B], OMT] with DeOptionizer[A, B, MT, Option[B], OMT] {
    val deOptionizer = jdbcMapper.asInstanceOf[TypedExpressionFactory[B, MT] with JdbcMapper[A, B]]
  }

  implicit val jdbcMapper = m.erasure match {
    case c if c == classOf[String] => new JdbcConverter[TString](dsl.stringTEF)
    case c if c == classOf[Date] => new JdbcConverter[TDate](dsl.dateTEF)
    case c if c == classOf[Timestamp] => new JdbcConverter[TTimestamp](dsl.timestampTEF)
    case c if c == classOf[Int] => new JdbcConverter[TInt](dsl.intTEF)
    case c if c == classOf[Long] => new JdbcConverter[TLong](dsl.longTEF)
    case c if c == classOf[Double] => new JdbcConverter[TDouble](dsl.doubleTEF)
    case c if c == classOf[Float] => new JdbcConverter[TFloat](dsl.floatTEF)
    case c if c == classOf[BigDecimal] => new JdbcConverter[TBigDecimal](dsl.bigDecimalTEF)
  }

  implicit val optionJdbcMapper = m.erasure match {
    case c if c == classOf[String] => new OptionConverter[String, TString, TOptionString]
    case c if c == classOf[Date] => new OptionConverter[Date, TDate, TOptionDate]
    case c if c == classOf[Timestamp] => new OptionConverter[Timestamp, TTimestamp, TOptionTimestamp]
    case c if c == classOf[Int] => new OptionConverter[Int, TInt, TOptionInt]
    case c if c == classOf[Long] => new OptionConverter[Long, TLong, TOptionLong]
    case c if c == classOf[Double] => new OptionConverter[Double, TDouble, TOptionDouble]
    case c if c == classOf[Float] => new OptionConverter[Float, TFloat, TOptionFloat]
    case c if c == classOf[BigDecimal] => new OptionConverter[BigDecimal, TDouble, TOptionBigDecimal]
  }

  Option(formConverter).foreach(f => FormConverter.register(m.erasure, f))
  ClassInfo.factories.register(m.erasure, {() => defaultValue.asInstanceOf[AnyRef]})
}

