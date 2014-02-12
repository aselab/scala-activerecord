package com.github.aselab.activerecord.squeryl

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import org.squeryl.dsl._
import java.util.{Date, UUID}
import java.sql.Timestamp
import reflections._

class ExpressionConversion(field: FieldInfo) {
  import ReflectionUtil._

  def toExpression(value: Any): dsl.TypedExpression[_, _] = field match {
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

  def toEqualityExpression(v1: => Any, v2: => Any): ast.EqualityExpression =
    new ast.EqualityExpression(toExpression(v1), toExpression(v2))

  def toOrderByExpression(v1: => Any, order: String): ExpressionNode = {
    val arg = new OrderByArg(toExpression(v1))
    order.toLowerCase match {
      case "asc" => arg.asc
      case "desc" => arg.desc
      case _ => throw new IllegalArgumentException("order must be 'asc' or 'desc'")
    }
  }

  def toInExpression(v1: Any, v2: List[Any]): ast.InclusionOperator = try {
    new ast.InclusionOperator(toExpression(v1), new ast.RightHandSideOfIn(new ast.ConstantExpressionNodeList(v2)))
  } catch {
    case e: RuntimeException => throw ActiveRecordException.unsupportedType(field.name)
  }
}
