package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import mojolly.inflector.InflectorImports._

trait RecordRelation

case class ActiveRecordOneToMany[M](override val relation: OneToMany[M]) extends StatefulOneToMany(relation) with RecordRelation
{
  
  override def refresh = dsl.inTransaction { super.refresh }

  override def associate(m: M)(implicit ev: M <:< KeyedEntity[_]) =
    dsl.inTransaction { super.associate(m) }

  override def deleteAll = dsl.inTransaction { super.deleteAll }
}

case class ActiveRecordManyToOne[O <: ActiveRecord](override val relation: ManyToOne[O]) extends StatefulManyToOne(relation) with RecordRelation
{
  
  override def refresh = dsl.inTransaction { super.refresh }

  override def assign(o: O) = dsl.inTransaction { super.assign(o) }

  override def delete = dsl.inTransaction { super.delete }
}

case class ActiveRecordManyToMany[O <: ActiveRecord, A <: KeyedEntity[_]](override val relation: ManyToMany[O, A]) extends StatefulManyToMany(relation) with RecordRelation
{
  override def refresh = dsl.inTransaction { super.refresh }

  override def associate(o: O, a: A) =
    dsl.inTransaction { super.associate(o, a) }
  
  override def associate(o: O) = dsl.inTransaction { super.associate(o) }

  override def dissociate(o: O) = dsl.inTransaction { super.dissociate(o) }

  override def dissociateAll = dsl.inTransaction { super.dissociateAll }
}

case class RelationWrapper[L <: ActiveRecord, R <: ActiveRecord](relation: Relation[L, R]) {
  def oneToManyRelation = relation.asInstanceOf[OneToManyRelation[L, R]]
  def manyToManyRelation = relation.asInstanceOf[ManyToManyRelation[L, R, KeyedEntity[_]]]

  def belongsTo(m: R) = ActiveRecordManyToOne(oneToManyRelation.right(m))
  def hasMany(m: L) = ActiveRecordOneToMany(oneToManyRelation.left(m))

  def hasAndBelongsToManyL(m: L) =
    ActiveRecordManyToMany(manyToManyRelation.left(m))
  def hasAndBelongsToManyR(m: R) =
    ActiveRecordManyToMany(manyToManyRelation.right(m))
}

trait RecordRelationSupport {
  private lazy val self = this.asInstanceOf[ActiveRecord]

  protected def relations: Map[(String, String), RelationWrapper[ActiveRecord, ActiveRecord]]

  private def getRelation(left: Class[_], right: Class[_]) =
    relations.get(left.getName -> right.getName)
     .getOrElse(ActiveRecordException.missingRelation)

  protected def belongsTo[T <: ActiveRecord](implicit m: Manifest[T]) =
    getRelation(m.erasure, getClass).belongsTo(self).asInstanceOf[ActiveRecordManyToOne[T]]

  protected def hasMany[T <: ActiveRecord](implicit m: Manifest[T]) =
    getRelation(getClass, m.erasure).hasMany(self).asInstanceOf[ActiveRecordOneToMany[T]]

  protected def hasAndBelongsToMany[T <: ActiveRecord](implicit m: Manifest[T])=
    relations.get(getClass.getName -> m.erasure.getName)
      .map(_.hasAndBelongsToManyL(self))
      .getOrElse(getRelation(m.erasure, getClass).hasAndBelongsToManyR(self))
      .asInstanceOf[ActiveRecordManyToMany[T, IntermediateRecord]]
}

trait TableRelationSupport extends Schema {
  import ReflectionUtil._
  type AR = com.github.aselab.activerecord.ActiveRecord

  lazy val relations = {
    this.getFields[Relation[AR, AR]].map {f =>
      val List(left, right, _*) = getGenericTypes(f).map(_.getName)
      val relation = this.getValue[Relation[AR, AR]](f.getName)

      (left, right) -> RelationWrapper(relation)
    }.toMap
  }

  def foreignKeyName(c: Class[_]) = c.getSimpleName.underscore.camelize + "Id"

  def foreignKeyIsOption(c: Class[_], name: String) = try {
    c.getDeclaredField(name).getType.getName == "scala.Option"
  } catch {
    case e: java.lang.NoSuchFieldException =>
      ActiveRecordException.missingForeignKey(name)
  }

  def oneToMany[O <: AR, M <: AR](ot: Table[O], mt:Table[M])(implicit om: Manifest[O], mm: Manifest[M]) = {
    val foreignKey = foreignKeyName(om.erasure)
    val isOption= foreignKeyIsOption(mm.erasure, foreignKey)

    oneToManyRelation(ot, mt).via {(o, m) => 
      if (isOption) {
        o.id === m.getValue[Option[Long]](foreignKey)
      } else {
        o.id === m.getValue[Long](foreignKey)
      }
    }
  }

  def manyToMany[L <: AR, R <: AR](lt: Table[L], rt:Table[R])(implicit lm: Manifest[L], rm: Manifest[R]) = {
    val foreignKeyL = foreignKeyName(lm.erasure)
    val foreignKeyR = foreignKeyName(rm.erasure)
    val middleName =
      lm.erasure.getSimpleName.pluralize + rm.erasure.getSimpleName.pluralize

    new ManyToManyRelationBuilder(lt, rt, Some(middleName))
      .via[IntermediateRecord] ((l, r, m) =>
        (l.id === m.leftId, r.id === m.rightId))
  }
}

case class IntermediateRecord(leftId: Long, rightId: Long)
  extends KeyedEntity[CompositeKey2[Long, Long]]
{
    def id = compositeKey(leftId, rightId)
}

