package com.github.aselab.activerecord.squeryl

import com.github.aselab.activerecord._
import dsl._
import org.squeryl.dsl._
import java.util.{Date, UUID}
import java.sql.Timestamp

class ExpressionConversion(field: FieldInfo) {
  import Implicits._

  def toExpression(value: Any): TypedExpression[_, _] = field match {
    case f if f.is[String] => value.toOption[String]
    case f if f.is[Boolean] => value.toOption[Boolean]
    case f if f.is[Int] => value.toOption[Int]
    case f if f.is[Long] => value.toOption[Long]
    case f if f.is[Float] => value.toOption[Float]
    case f if f.is[Double] => value.toOption[Double]
    case f if f.is[BigDecimal] => value.toOption[BigDecimal]
    case f if f.is[Timestamp] => value.toOption[Timestamp]
    case f if f.is[Date] => value.toOption[Date]
    case f if f.is[UUID] => value.toOption[UUID]
  }

  def toEqualityExpression(v1: Any, v2: Any): ast.EqualityExpression = try {
    if (!field.isOption && (v2 == null || v2 == None)) throw new Exception
    new ast.EqualityExpression(toExpression(v1), toExpression(v2))
  } catch {
    case e => throw ActiveRecordException.unsupportedType(
      field.name + " by " + Option(v2).map(_.toString).getOrElse("null")
    )
  }
}
