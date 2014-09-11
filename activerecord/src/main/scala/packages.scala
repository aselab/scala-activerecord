package com.github.aselab.activerecord

import ActiveRecord._
import reflections._

object dsl extends org.squeryl.PrimitiveTypeMode
  with inner.Annotations with inner.DSL with inner.Types {
  val optionUUIDTEF = PrimitiveTypeSupport.optionUUIDTEF
  val optionBooleanTEF = PrimitiveTypeSupport.optionBooleanTEF
  private def deprecateMessage(s: String) =
    s"dsl#${s} is deprecated and causes runtime error. use ActiveRecordCompanion#${s} instead."

  @deprecated(deprecateMessage("transaction"), "0.3.0")
  override def transaction[A](a: => A): A = sys.error(deprecateMessage("transaction"))
  override def transaction[A](sf: org.squeryl.SessionFactory)(a: => A): A = super.transaction(sf)(a)
  @deprecated(deprecateMessage("inTransaction"), "0.3.0")
  override def inTransaction[A](a: => A): A = sys.error(deprecateMessage("inTransaction"))
  override def inTransaction[A](sf: org.squeryl.SessionFactory)(a: => A): A = super.inTransaction(sf)(a)
}

package views {
  object dsl extends org.squeryl.PrimitiveTypeMode with inner.DSL
}

package object aliases {
  type AR = ActiveRecordBase[_]
  type ARC = ActiveRecordBaseCompanion[_, _]
  type CKey = dsl.CompositeKey2[Long, Long]
}

package object support {
  import ReflectionUtil._
  type PF = PartialFunction[String, Class[_]]

  val primitiveClasses: PF = {
    case "scala.Predef.String" | "java.lang.String" => classOf[String]
    case "boolean" | "scala.Boolean" | "java.lang.Boolean" => classOf[Boolean]
    case "int" | "scala.Int" | "java.lang.Integer" => classOf[Int]
    case "long" | "scala.Long" | "java.lang.Long" => classOf[Long]
    case "float" | "scala.Float" | "java.lang.Float" => classOf[Float]
    case "double" | "scala.Double" | "java.lang.Double" => classOf[Double]
    case "scala.package.BigDecimal" | "scala.math.BigDecimal" => classOf[BigDecimal]
    case "java.sql.Timestamp" => classOf[java.sql.Timestamp]
    case "java.util.Date" => classOf[java.util.Date]
    case "java.util.UUID" => classOf[java.util.UUID]
  }
}
