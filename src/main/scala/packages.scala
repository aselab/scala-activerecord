package com.github.aselab.activerecord

object dsl extends org.squeryl.PrimitiveTypeMode with Annotations

package object support {
  val primitiveClasses: PartialFunction[String, Class[_]] = {
    case "scala.Predef.String" | "java.lang.String" => classOf[String]
    case "scala.Boolean" | "java.lang.Boolean" => classOf[Boolean]
    case "scala.Int" | "java.lang.Integer" => classOf[Int]
    case "scala.Long" | "java.lang.Long" => classOf[Long]
    case "scala.Float" | "java.lang.Float" => classOf[Float]
    case "scala.Double" | "java.lang.Double" => classOf[Double]
    case "scala.package.BigDecimal" | "scala.math.BigDecimal" => classOf[BigDecimal]
    case "java.sql.Timestamp" => classOf[java.sql.Timestamp]
    case "java.util.Date" => classOf[java.util.Date]
    case "java.util.UUID" => classOf[java.util.UUID]
  }

  val modelClass = new PartialFunction[String, Class[_]] {
    val c = classOf[ProductModel]

    def apply(s: String) = s match {
      case s if isDefinedAt(s) => Class.forName(s)
    }

    def isDefinedAt(s: String) = try {
      c.isAssignableFrom(Class.forName(s))
    } catch {
      case e => false
    }
  }
}
