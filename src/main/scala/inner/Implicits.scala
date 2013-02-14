package com.github.aselab.activerecord.inner

import org.squeryl._
import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import ActiveRecord._

// low priority implicits
trait CompanionIterable[T <: AR] {
  implicit val manifest: Manifest[T]
  def all: Relation1[T, T]
  implicit def companionToIterable(c: this.type) = c.all.toList
}

trait CompanionConversion[T <: AR] extends CompanionIterable[T] {
  implicit def companionToRelation(c: this.type) = c.all
}

// low priority implicits
trait IterableConversion { self: PrimitiveTypeMode =>
  implicit def relationToIterable[T](relation: Relation[_, T])
    (implicit m: Manifest[T]): Iterable[T] = relation.load

  implicit def associationToIterable[T <: AR]
    (association: Association[_, T])(implicit m: Manifest[T]): Iterable[T] =
      relationToIterable(association.relation)
}

trait DSL extends IterableConversion { self: PrimitiveTypeMode =>
  implicit def keyedEntityDef[T <: AR](implicit m: Manifest[T]) = {
    ReflectionUtil.classToCompanion(m.erasure.getName)
      .asInstanceOf[ActiveRecordBaseCompanion[_, T]]
      .keyedEntityDef.asInstanceOf[KeyedEntityDef[T, _]]
  }

  implicit def queryToRelation[T <: AR](query: Queryable[T])
    (implicit m: Manifest[T]): Relation1[T, T] =
    Relation(query, identity)

  implicit def belongsToAssociationToOption[T <: AR]
    (association: BelongsToAssociation[_, T]): Option[T] = association.toOption

  implicit def belongsToAssociationToRecord[T <: AR]
    (association: BelongsToAssociation[_, T]): T = association.relation.head

  implicit def associationToRelation1[T <: AR]
    (association: {def relation1: Relation1[T, T]}) = association.relation1

  implicit def associationToRelation2[T <: AR, I <: AR]
    (association: {def relation2: Relation2[T, I, T]}) = association.relation2
}
