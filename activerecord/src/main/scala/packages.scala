package com.github.aselab.activerecord

import ActiveRecord._
import reflections._

object dsl extends org.squeryl.PrimitiveTypeMode
  with inner.Annotations with inner.DSL with inner.Types

package views {
  object dsl extends org.squeryl.PrimitiveTypeMode with inner.DSL
}

package object aliases {
  type AR = ActiveRecordBase[_]
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

  def modelClass(implicit classLoader: ClassLoader = defaultLoader): PF =
    new PF {
      val c = classOf[inner.ProductModel]

      def apply(s: String): Class[_] = s match {
        case s if isDefinedAt(s) => ReflectionUtil.loadClass(s)
      }

      def isDefinedAt(s: String): Boolean = try {
        c.isAssignableFrom(ReflectionUtil.loadClass(s))
      } catch {
        case e: ClassNotFoundException => false
      }
    }

  def allClasses(implicit classLoader: ClassLoader = defaultLoader): PF =
    primitiveClasses.orElse(modelClass)
}
