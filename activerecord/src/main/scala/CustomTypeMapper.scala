package com.github.aselab.activerecord

import io._
import reflections._
import java.util.Date
import java.sql.Timestamp
import org.squeryl.dsl._
import scala.reflect._
import scala.reflect.runtime.universe._

abstract class CustomTypeMapper[A: TypeTag, B: ClassTag](primitiveTypeMode: org.squeryl.PrimitiveTypeMode) {
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

  class OptionConverter[T, MT, OMT](implicit mapper: JdbcConverter[MT])
    extends TypedExpressionFactory[Option[B], OMT]
    with DeOptionizer[A, B, MT, Option[B], OMT] {
    val deOptionizer = mapper
  }

  import primitiveTypeMode._
  private lazy val _string = new JdbcConverter[TString](stringTEF)
  private lazy val _date = new JdbcConverter[TDate](dateTEF)
  private lazy val _timestamp = new JdbcConverter[TTimestamp](timestampTEF)
  private lazy val _int = new JdbcConverter[TInt](intTEF)
  private lazy val _long = new JdbcConverter[TLong](longTEF)
  private lazy val _double = new JdbcConverter[TDouble](doubleTEF)
  private lazy val _float = new JdbcConverter[TFloat](floatTEF)
  private lazy val _bigDecimal = new JdbcConverter[TBigDecimal](bigDecimalTEF)

  implicit def string(implicit ev: A =:= String) = _string
  implicit def date(implicit ev: A =:= Date) = _date
  implicit def timestamp(implicit ev: A =:= Timestamp) = _timestamp
  implicit def int(implicit ev: A =:= Int) = _int
  implicit def long(implicit ev: A =:= Long) = _long
  implicit def double(implicit ev: A =:= Double) = _double
  implicit def float(implicit ev: A =:= Float) = _float
  implicit def bigDecimal(implicit ev: A =:= BigDecimal) = _bigDecimal

  implicit def stringOption(implicit ev: A =:= String) =
    new OptionConverter[String, TString, TOptionString]
  implicit def dateOption(implicit ev: A =:= Date) =
    new OptionConverter[Date, TDate, TOptionDate]
  implicit def timestampOption(implicit ev: A =:= Timestamp) =
    new OptionConverter[Timestamp, TTimestamp, TOptionTimestamp]
  implicit def intOption(implicit ev: A =:= Int) =
    new OptionConverter[Int, TInt, TOptionInt]
  implicit def longOption(implicit ev: A =:= Long) =
    new OptionConverter[Long, TLong, TOptionLong]
  implicit def doubleOption(implicit ev: A =:= Double) =
    new OptionConverter[Double, TDouble, TOptionDouble]
  implicit def floatOption(implicit ev: A =:= Float) =
    new OptionConverter[Float, TFloat, TOptionFloat]
  implicit def bigDecimalOption(implicit ev: A =:= BigDecimal) =
    new OptionConverter[BigDecimal, TBigDecimal, TOptionBigDecimal]

  // register JDBC mapper in NonPrimitiveJdbcMapper initilized
  // https://github.com/squeryl/squeryl/blob/0.9.6-RC3/src/main/scala/org/squeryl/dsl/TypedExpression.scala#L291
  private val _register = typeTag[A] match {
    case t if t.tpe =:= typeOf[String] => _string
    case t if t.tpe =:= typeOf[Date] => _date
    case t if t.tpe =:= typeOf[Timestamp] => _timestamp
    case t if t.tpe =:= typeOf[Int] => _int
    case t if t.tpe =:= typeOf[Long] => _long
    case t if t.tpe =:= typeOf[Double] => _double
    case t if t.tpe =:= typeOf[Float] => _float
    case t if t.tpe =:= typeOf[BigDecimal] => _bigDecimal
  }

  Option(formConverter).foreach(f => FormConverter.register(classTag[B].runtimeClass, f))
  ClassInfo.factories.register(classTag[B].runtimeClass, {() => defaultValue.asInstanceOf[AnyRef]})
}

