package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import ActiveRecord._
import reflections._
import scala.reflect.ClassTag
import scala.language.reflectiveCalls

// scalastyle:off

// low priority implicits
trait CompanionIterable[T <: AR] {
  implicit val manifest: ClassTag[T]
  def all: Relation1[T, T]
  implicit def companionToIterable(c: this.type): List[T] = c.all.toList
}

trait CompanionConversion[T <: AR] extends CompanionIterable[T] {
  implicit def companionToRelation(c: this.type) = c.all
}

// low priority implicits
trait IterableConversion {
  implicit def relationToIterable[T](relation: Relation[_, T])
    (implicit manifest: ClassTag[T]): Iterable[T] = relation.load

  implicit def associationToIterable[T <: AR]
    (association: Association[_, T])(implicit manifest: ClassTag[T]): Iterable[T] =
      relationToIterable(association.relation)
}

trait DSL extends IterableConversion {
  implicit def keyedEntityDef[T <: AR](implicit manifest: ClassTag[T]) = {
    ReflectionUtil.classToARCompanion[T](manifest.runtimeClass)
      .keyedEntityDef.asInstanceOf[KeyedEntityDef[T, _]]
  }

  implicit def queryToRelation[T <: AR](query: Queryable[T])
    (implicit m: ClassTag[T]): Relation1[T, T] =
    Relation1(Parameters[T, Tuple1[T], T](), query)

  implicit def relationToQuery[T <: AR, S]
    (relation: Relation[T, S]): Query[S] = relation.toQuery

  implicit def belongsToAssociationToOption[T <: AR]
    (association: BelongsToAssociation[_, T]): Option[T] = association.toOption

  implicit def belongsToAssociationToRecord[T <: AR]
    (association: BelongsToAssociation[_, T]): T = association.relation.head

  implicit def singularAssociationToOption[T <: AR]
    (association: SingularAssociation[_, T]): Option[T] = association.toOption

  implicit def singularAssociationToRecord[T <: AR]
    (association: SingularAssociation[_, T]): T = association.relation.head

  implicit def associationToRelation1[T <: AR]
    (association: {def relation1: Relation1[T, T]}) = association.relation1

  implicit def associationToRelation2[T <: AR, I <: AR]
    (association: {def relation2: Relation2[T, I, T]}) = association.relation2
}
