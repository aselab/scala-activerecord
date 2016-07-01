package com.github.aselab.activerecord

import scala.language.experimental.macros
import org.joda.time.{LocalDate, DateTime}

object dsl extends org.squeryl.PrimitiveTypeMode
  with squeryl.DateTimeSupport with squeryl.LocalDateSupport
  with inner.Annotations with inner.DSL with inner.Types with io.JsonImplicits {
  val optionUUIDTEF = PrimitiveTypeSupport.optionUUIDTEF
  val optionBooleanTEF = PrimitiveTypeSupport.optionBooleanTEF

  override def transaction[A](a: => A): A = macro MethodMacros.unsupportedInTransaction[A]
  override def transaction[A](sf: org.squeryl.SessionFactory)(a: => A): A = super.transaction(sf)(a)
  override def inTransaction[A](a: => A): A = macro MethodMacros.unsupportedInTransaction[A]
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
    case "org.joda.time.DateTime" => classOf[DateTime]
    case "org.joda.time.LocalDate" => classOf[LocalDate]
    case "java.util.UUID" => classOf[java.util.UUID]
  }
}
