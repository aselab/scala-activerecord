package com.github.aselab.activerecord

import org.squeryl._

object dsl extends PrimitiveTypeMode with Annotations {
  import ActiveRecord._

  implicit def keyedEntityDef[T <: ActiveRecordBase[_]](implicit m: Manifest[T]) = {
    ReflectionUtil.classToCompanion(m.erasure.getName)
      .asInstanceOf[ActiveRecordBaseCompanion[_, T]]
      .keyedEntityDef.asInstanceOf[KeyedEntityDef[T, _]]
  }

  implicit def relationToIterable[T](relation: Relation[_, T])
    (implicit m: Manifest[T]): Iterable[T] =
    inTransaction { queryToIterable(relation.toQuery).toList }

  implicit def companionToRelation[T <: ActiveRecordBase[_]]
    (c: ActiveRecordBaseCompanion[_, T])(implicit m: Manifest[T]): Relation1[T, T] =
    queryToRelation[T](c.table)

  implicit def companionToIterable[T <: ActiveRecordBase[_]]
    (c: ActiveRecordBaseCompanion[_, T])(implicit m: Manifest[T]): Iterable[T] =
    relationToIterable(c)

  implicit def queryToRelation[T <: ActiveRecordBase[_]](query: Queryable[T])
    (implicit m: Manifest[T]): Relation1[T, T] =
    Relation(query, identity)

  implicit def associationToRelation1[T <: ActiveRecordBase[_]]
    (association: BelongsToAssociation[_, T]) = association.relation

  implicit def associationToRelation2[T <: ActiveRecordBase[_]]
    (association: HasManyAssociation[_, T]) = association.relation

  implicit def associationToIterable[T <: ActiveRecordBase[_]]
    (association: Association[_, T])(implicit m: Manifest[T]): Iterable[T] =
    association match {
      case a: BelongsToAssociation[_, T] => relationToIterable(a.relation)
      case a: HasManyAssociation[_, T] => relationToIterable(a.relation)
      case a: HasManyThroughAssociation[_, T, _] => relationToIterable(a.relation)
    }

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
      val c = classOf[ProductModel]

      def apply(s: String): Class[_] = s match {
        case s if isDefinedAt(s) => ReflectionUtil.loadClass(s)
      }

      def isDefinedAt(s: String): Boolean = try {
        c.isAssignableFrom(ReflectionUtil.loadClass(s))
      } catch {
        case e => false
      }
    }

  def allClasses(implicit classLoader: ClassLoader = defaultLoader): PF =
    primitiveClasses.orElse(modelClass)
}
