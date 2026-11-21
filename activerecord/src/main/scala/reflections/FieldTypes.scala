package com.github.aselab.activerecord.reflections

import scala.collection.concurrent.TrieMap
import scala.quoted.*
import scala.reflect.ClassTag

/**
 * Element classes of a model's fields that have a single type argument of a supported value type,
 * such as `Option[Int]` or `List[Long]`, keyed by field name.
 *
 * The JVM erases `Option[Int]` to `Option<Object>`, so Java reflection cannot recover `Int`.
 * Scala 2 read it back from the ScalaSig attribute at run time; Scala 3 has no such attribute,
 * so the information is captured at compile time instead. Model companions
 * (`ActiveModelCompanion`, `ActiveRecordCompanion`, ...) receive it automatically and register it
 * for their model class; [[FieldTypes.register]] does the same for classes without a companion.
 */
final class FieldTypes[T](val types: Map[String, Class[?]])

object FieldTypes {
  private val registry = TrieMap.empty[Class[?], Map[String, Class[?]]]

  /** Field types of `T`, computed at compile time. */
  inline given derived[T]: FieldTypes[T] = ${ derivedImpl[T] }

  /** Registers the field types of `T` for its run-time class. */
  inline def register[T](using ct: ClassTag[T]): Unit = register(ct.runtimeClass, derived[T].types)

  def register(clazz: Class[?], types: Map[String, Class[?]]): Unit = registry.update(clazz, types)

  /** Field types registered for `clazz`, if any. */
  def lookup(clazz: Class[?]): Option[Map[String, Class[?]]] = registry.get(clazz)

  private def derivedImpl[T: Type](using Quotes): Expr[FieldTypes[T]] = {
    import quotes.reflect.*

    def classOfExpr(name: String): Option[Expr[Class[?]]] = name match {
      case "java.lang.String" => Some('{ classOf[String] })
      case "scala.Boolean" | "java.lang.Boolean" => Some('{ classOf[Boolean] })
      case "scala.Int" | "java.lang.Integer" => Some('{ classOf[Int] })
      case "scala.Long" | "java.lang.Long" => Some('{ classOf[Long] })
      case "scala.Float" | "java.lang.Float" => Some('{ classOf[Float] })
      case "scala.Double" | "java.lang.Double" => Some('{ classOf[Double] })
      case "scala.math.BigDecimal" => Some('{ classOf[BigDecimal] })
      case "java.sql.Timestamp" => Some('{ classOf[java.sql.Timestamp] })
      case "java.util.Date" => Some('{ classOf[java.util.Date] })
      case "org.joda.time.DateTime" => Some('{ classOf[org.joda.time.DateTime] })
      case "org.joda.time.LocalDate" => Some('{ classOf[org.joda.time.LocalDate] })
      case "java.util.UUID" => Some('{ classOf[java.util.UUID] })
      case _ => None
    }

    val tpe = TypeRepr.of[T]
    // declared fields of the type and all its base classes; the most specific declaration wins
    val fields = tpe.baseClasses.flatMap(_.declaredFields).distinctBy(_.name.trim)
    val entries = fields.flatMap { f =>
      tpe.memberType(f).widen.dealias match {
        case AppliedType(_, List(arg)) =>
          classOfExpr(arg.dealias.typeSymbol.fullName).map(c => '{ (${ Expr(f.name.trim) }, $c) })
        case _ => None
      }
    }
    '{ new FieldTypes[T](Map(${ Varargs(entries) }*)) }
  }
}
