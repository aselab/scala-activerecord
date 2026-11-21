package com.github.aselab.activerecord.squeryl

import org.squeryl.dsl._
import com.github.aselab.activerecord.dsl
import java.sql.Timestamp
import java.util.Date
import org.joda.time.{DateTime, LocalDate}

/*
 * Squeryl's aggregate functions, e.g. `max[T2 >: TOption, T1 <: T2, A1, A2](b)(implicit f: TypedExpressionFactory[A2, T2])`,
 * rely on Scala 2 instantiating T2 to the least upper bound (TOptionLong for a Long column) before searching
 * for `f`. Scala 3 searches with T2 still open, so several factories match and the call is ambiguous.
 * These type classes pick the factory from the column's value type A1 instead, giving the same result
 * types as on Scala 2.
 */

/** Result of `max` / `min` on a column of type A1: `Option` of the column type. */
final class MaxMinResult[A1, A2, T2](val factory: TypedExpressionFactory[A2, T2])

object MaxMinResult {
  private def apply[A1, A2, T2](f: TypedExpressionFactory[A2, T2]) = new MaxMinResult[A1, A2, T2](f)

  given byte: MaxMinResult[Byte, Option[Byte], TOptionByte] = apply(dsl.optionByteTEF)
  given optionByte: MaxMinResult[Option[Byte], Option[Byte], TOptionByte] = apply(dsl.optionByteTEF)
  given int: MaxMinResult[Int, Option[Int], TOptionInt] = apply(dsl.optionIntTEF)
  given optionInt: MaxMinResult[Option[Int], Option[Int], TOptionInt] = apply(dsl.optionIntTEF)
  given long: MaxMinResult[Long, Option[Long], TOptionLong] = apply(dsl.optionLongTEF)
  given optionLong: MaxMinResult[Option[Long], Option[Long], TOptionLong] = apply(dsl.optionLongTEF)
  given float: MaxMinResult[Float, Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given optionFloat: MaxMinResult[Option[Float], Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given double: MaxMinResult[Double, Option[Double], TOptionDouble] = apply(dsl.optionDoubleTEF)
  given optionDouble: MaxMinResult[Option[Double], Option[Double], TOptionDouble] = apply(dsl.optionDoubleTEF)
  given bigDecimal: MaxMinResult[BigDecimal, Option[BigDecimal], TOptionBigDecimal] = apply(dsl.optionBigDecimalTEF)
  given optionBigDecimal: MaxMinResult[Option[BigDecimal], Option[BigDecimal], TOptionBigDecimal] = apply(dsl.optionBigDecimalTEF)
  given string: MaxMinResult[String, Option[String], TOptionString] = apply(dsl.optionStringTEF)
  given optionString: MaxMinResult[Option[String], Option[String], TOptionString] = apply(dsl.optionStringTEF)
  given date: MaxMinResult[Date, Option[Date], TOptionDate] = apply(dsl.optionDateTEF)
  given optionDate: MaxMinResult[Option[Date], Option[Date], TOptionDate] = apply(dsl.optionDateTEF)
  given sqlDate: MaxMinResult[java.sql.Date, Option[java.sql.Date], TOptionDate] = apply(dsl.optionSqlDateTEF)
  given optionSqlDate: MaxMinResult[Option[java.sql.Date], Option[java.sql.Date], TOptionDate] = apply(dsl.optionSqlDateTEF)
  given timestamp: MaxMinResult[Timestamp, Option[Timestamp], TOptionTimestamp] = apply(dsl.optionTimestampTEF)
  given optionTimestamp: MaxMinResult[Option[Timestamp], Option[Timestamp], TOptionTimestamp] = apply(dsl.optionTimestampTEF)
  given jodaDateTime: MaxMinResult[DateTime, Option[DateTime], TOptionTimestamp] = apply(dsl.optionJodaTimeTEF)
  given optionJodaDateTime: MaxMinResult[Option[DateTime], Option[DateTime], TOptionTimestamp] = apply(dsl.optionJodaTimeTEF)
  given jodaLocalDate: MaxMinResult[LocalDate, Option[LocalDate], TOptionDate] = apply(dsl.optionLocalDateTEF)
  given optionJodaLocalDate: MaxMinResult[Option[LocalDate], Option[LocalDate], TOptionDate] = apply(dsl.optionLocalDateTEF)
}

/** Result of `sum` on a numeric column of type A1: `Option` of the column type. */
final class SumResult[A1, A2, T2](val factory: TypedExpressionFactory[A2, T2])

object SumResult {
  private def apply[A1, A2, T2](f: TypedExpressionFactory[A2, T2]) = new SumResult[A1, A2, T2](f)

  given byte: SumResult[Byte, Option[Byte], TOptionByte] = apply(dsl.optionByteTEF)
  given optionByte: SumResult[Option[Byte], Option[Byte], TOptionByte] = apply(dsl.optionByteTEF)
  given int: SumResult[Int, Option[Int], TOptionInt] = apply(dsl.optionIntTEF)
  given optionInt: SumResult[Option[Int], Option[Int], TOptionInt] = apply(dsl.optionIntTEF)
  given long: SumResult[Long, Option[Long], TOptionLong] = apply(dsl.optionLongTEF)
  given optionLong: SumResult[Option[Long], Option[Long], TOptionLong] = apply(dsl.optionLongTEF)
  given float: SumResult[Float, Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given optionFloat: SumResult[Option[Float], Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given double: SumResult[Double, Option[Double], TOptionDouble] = apply(dsl.optionDoubleTEF)
  given optionDouble: SumResult[Option[Double], Option[Double], TOptionDouble] = apply(dsl.optionDoubleTEF)
  given bigDecimal: SumResult[BigDecimal, Option[BigDecimal], TOptionBigDecimal] = apply(dsl.optionBigDecimalTEF)
  given optionBigDecimal: SumResult[Option[BigDecimal], Option[BigDecimal], TOptionBigDecimal] = apply(dsl.optionBigDecimalTEF)
}

/**
 * Result of `avg` on a numeric column of type A1: `Option[Float]` for Byte, Int and Float,
 * `Option[Double]` for Long and Double, `Option[BigDecimal]` for BigDecimal.
 */
final class AvgResult[A1, A2, T2](val factory: TypedExpressionFactory[A2, T2])

object AvgResult {
  private def apply[A1, A2, T2](f: TypedExpressionFactory[A2, T2]) = new AvgResult[A1, A2, T2](f)

  given byte: AvgResult[Byte, Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given optionByte: AvgResult[Option[Byte], Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given int: AvgResult[Int, Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given optionInt: AvgResult[Option[Int], Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given float: AvgResult[Float, Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given optionFloat: AvgResult[Option[Float], Option[Float], TOptionFloat] = apply(dsl.optionFloatTEF)
  given long: AvgResult[Long, Option[Double], TOptionDouble] = apply(dsl.optionDoubleTEF)
  given optionLong: AvgResult[Option[Long], Option[Double], TOptionDouble] = apply(dsl.optionDoubleTEF)
  given double: AvgResult[Double, Option[Double], TOptionDouble] = apply(dsl.optionDoubleTEF)
  given optionDouble: AvgResult[Option[Double], Option[Double], TOptionDouble] = apply(dsl.optionDoubleTEF)
  given bigDecimal: AvgResult[BigDecimal, Option[BigDecimal], TOptionBigDecimal] = apply(dsl.optionBigDecimalTEF)
  given optionBigDecimal: AvgResult[Option[BigDecimal], Option[BigDecimal], TOptionBigDecimal] = apply(dsl.optionBigDecimalTEF)
}

/** Aggregate functions that resolve their result type through the type classes above. */
object Aggregates {
  /** Squeryl's DSL without the overloads below, to call its aggregate functions unambiguously */
  private object P extends org.squeryl.PrimitiveTypeMode

  // Squeryl's marker types are phantom: casting them only satisfies its bounds, the factory decides the result.
  def max[A1, T1, A2, T2](b: TypedExpression[A1, T1])(implicit r: MaxMinResult[A1, A2, T2]): TypedExpressionConversion[A2, T2] =
    P.max[TOption, TOption, A1, A2](b.asInstanceOf[TypedExpression[A1, TOption]])(
      using r.factory.asInstanceOf[TypedExpressionFactory[A2, TOption]]).asInstanceOf[TypedExpressionConversion[A2, T2]]

  def min[A1, T1, A2, T2](b: TypedExpression[A1, T1])(implicit r: MaxMinResult[A1, A2, T2]): TypedExpressionConversion[A2, T2] =
    P.min[TOption, TOption, A1, A2](b.asInstanceOf[TypedExpression[A1, TOption]])(
      using r.factory.asInstanceOf[TypedExpressionFactory[A2, TOption]]).asInstanceOf[TypedExpressionConversion[A2, T2]]

  def sum[A1, T1 >: TNumericLowerTypeBound, A2, T2](b: TypedExpression[A1, T1])(implicit r: SumResult[A1, A2, T2]): TypedExpressionConversion[A2, T2] =
    P.sum[TOptionBigDecimal, TOptionBigDecimal, A1, A2](b.asInstanceOf[TypedExpression[A1, TOptionBigDecimal]])(
      using r.factory.asInstanceOf[TypedExpressionFactory[A2, TOptionBigDecimal]]).asInstanceOf[TypedExpressionConversion[A2, T2]]

  def avg[A1, T1, A2, T2](b: TypedExpression[A1, T1])(implicit r: AvgResult[A1, A2, T2]): TypedExpressionConversion[A2, T2] =
    P.avg[TOptionFloat, TOptionFloat, A1, A2](b.asInstanceOf[TypedExpression[A1, TOptionFloat]])(
      using r.factory.asInstanceOf[TypedExpressionFactory[A2, TOptionFloat]]).asInstanceOf[TypedExpressionConversion[A2, T2]]
}

/**
 * Mixed into `dsl`: overloads of Squeryl's aggregate functions that compile on Scala 3.
 * They are chosen over Squeryl's own because they are defined in a subclass of QueryDsl
 * and are otherwise exactly as specific (same result type, same bound on T1 for `sum`).
 */
trait AggregateFunctions extends org.squeryl.PrimitiveTypeMode {
  def max[A1, T1, A2, T2](b: TypedExpression[A1, T1])(implicit r: MaxMinResult[A1, A2, T2]): TypedExpressionConversion[A2, T2] =
    Aggregates.max(b)

  def min[A1, T1, A2, T2](b: TypedExpression[A1, T1])(implicit r: MaxMinResult[A1, A2, T2]): TypedExpressionConversion[A2, T2] =
    Aggregates.min(b)

  def sum[A1, T1 >: TNumericLowerTypeBound, A2, T2](b: TypedExpression[A1, T1])(implicit r: SumResult[A1, A2, T2]): TypedExpressionConversion[A2, T2] =
    Aggregates.sum(b)

  def avg[A1, T1, A2, T2](b: TypedExpression[A1, T1])(implicit r: AvgResult[A1, A2, T2]): TypedExpressionConversion[A2, T2] =
    Aggregates.avg(b)
}
