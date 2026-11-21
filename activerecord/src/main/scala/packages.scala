package com.github.aselab.activerecord

import scala.annotation.compileTimeOnly
import org.joda.time.{LocalDate, DateTime}

trait dsl extends org.squeryl.PrimitiveTypeMode
  with squeryl.AggregateFunctions
  with squeryl.DateTimeSupport with squeryl.LocalDateSupport
  with inner.DSL with io.JsonImplicits {
  val optionUUIDTEF = PrimitiveTypeSupport.optionUUIDTEF
  val optionBooleanTEF = PrimitiveTypeSupport.optionBooleanTEF

  @compileTimeOnly("dsl#inTransaction is deprecated. use ActiveRecordCompanion#inTransaction instead.")
  override def transaction[A](a: => A): A = super.transaction(a)
  override def transaction[A](sf: org.squeryl.SessionFactory)(a: => A): A = super.transaction(sf)(a)
  @compileTimeOnly("dsl#inTransaction is deprecated. use ActiveRecordCompanion#inTransaction instead.")
  override def inTransaction[A](a: => A): A = super.inTransaction(a)
  override def inTransaction[A](sf: org.squeryl.SessionFactory)(a: => A): A = super.inTransaction(sf)(a)
}

object dsl extends dsl with inner.Annotations with inner.Types

package views {
  /**
   * Adding additional imports to templates for Play Framework 2.x.
   * Add this line to `build.sbt`:
   * {{{
   * TwirlKeys.templateImports ++= Seq("com.github.aselab.activerecord.views.dsl._")
   * }}}
   */
  object dsl extends dsl
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
