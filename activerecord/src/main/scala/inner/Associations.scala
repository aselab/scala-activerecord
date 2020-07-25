package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import squeryl.Implicits._
import reflections._
import ReflectionUtil._
import ActiveRecord._
import scala.language.existentials
import scala.reflect.ClassTag

trait Associations {
  trait Association[+O <: AR, T <: AR] {
    val owner: O
    val associationClass = manifest.runtimeClass
    implicit val manifest: ClassTag[T]

    def foreignKey: String

    def relation: Relation[T, T]

    protected[inner] def eagerLoad[S <: AR](sources: List[S])
      (implicit m: ClassTag[S]): Map[Any, List[T]]

    protected lazy val companion = classToARCompanion[T](associationClass)

    def inTransaction[A](a: => A): A = companion.inTransaction(a)

    protected lazy val source: Relation1[T, T] = companion.table

    protected[inner] def fieldInfo(name: String) =
      companion.fieldInfo.getOrElse(name, throw ActiveRecordException.notFoundField(name))
  }

  trait OwnersAssociation[O <: AR, T <: AR] extends Association[O, T] {
    val allConditions: Map[String, Any]

    protected def hasConstraint: Boolean

    protected def conditionFactory(conditions: Map[String, Any]) = {
      m: T => LogicalBoolean.and(conditions.map {
        case (key, value) =>
          fieldInfo(key).toEqualityExpression(m.getValue[Any](key), value)
      }.toSeq)
    }

    def condition: T => LogicalBoolean = conditionFactory(allConditions)

    def build: T = assignConditions(companion.newInstance)

    protected def assignConditions(m: T): T = {
      if (owner.isNewRecord) throw ActiveRecordException.recordMustBeSaved
      allConditions.foreach {
        case (key, value) => fieldInfo(key).setValue(m, value)
      }
      m
    }
  }

  trait SingularAssociation[O <: AR, T <: AR] extends OwnersAssociation[O, T] {
    def toOption: Option[T] = relation.headOption

    def remove(): Option[T]

    def delete(): Option[T] = companion.inTransaction {
      val result = toOption
      result.foreach(_.delete())
      relation.cache = Nil
      result
    }
  }

  trait CollectionAssociation[O <: AR, T <: AR] extends OwnersAssociation[O, T]{
    def remove(m: T): Option[T]

    def removeAll(): List[T]

    def deleteAll(): List[T] = companion.inTransaction {
      val result = relation.toList
      result.foreach(_.delete())
      relation.cache = Nil
      result
    }
  }

  class BelongsToAssociation[O <: AR, T <: AR](
    val owner: O, val foreignKey: String
  )(implicit val manifest: ClassTag[T]) extends Association[O, T] {
    lazy val foreignKeyInfo = owner._companion.fieldInfo(foreignKey)

    def condition: T => LogicalBoolean =
      m => foreignKeyInfo.toEqualityExpression(m.id, owner.getValue[Any](foreignKey))

    def eagerLoad[S <: AR](sources: List[S])
      (implicit m: ClassTag[S]): Map[Any, List[T]] = {
      val ids = sources.map(_.id)
      val field = foreignKeyInfo
      val r = source.joins[S]((m, o) =>
        field.toEqualityExpression(m.id, o.getValue[Any](foreignKey))
      ).where((m, o) => field.toInExpression(o.id, ids)).toQuery.toList
      val map = r.groupBy(_.id)
      sources.map(r => (r.id, map.getOrElse(r.getOption[Any](foreignKey).orNull, Nil))).toMap
    }

    lazy val relation1: Relation1[T, T] = source.where(condition).limit(1)

    def relation: Relation[T, T] = relation1

    def toOption: Option[T] = relation.headOption

    def assign(m: T): T = {
      if (m.isNewRecord) throw ActiveRecordException.recordMustBeSaved
      foreignKeyInfo.setValue(owner, m.id)
      relation.cache = List(m)
      m
    }

    def associate(m: T): T = assign(m).update

    def :=(m: T): T = assign(m)
  }

  class HasOneAssociation[O <: AR, T <: AR](
    val owner: O, conditions: Map[String, Any], val foreignKey: String
  )(implicit val manifest: ClassTag[T]) extends SingularAssociation[O, T] {
    val allConditions = conditions + (foreignKey -> owner.id)

    protected lazy val hasConstraint = !fieldInfo(foreignKey).isOption

    lazy val relation1: Relation1[T, T] = source.where(condition).limit(1)

    def relation: Relation[T, T] = relation1

    def eagerLoad[S <: AR](sources: List[S])
      (implicit m: ClassTag[S]): Map[Any, List[T]] = {
      val ids = sources.map(_.id)
      val field = fieldInfo(foreignKey)

      val r = source.where(conditionFactory(conditions)).where(
        m => field.toInExpression(m.getValue[Any](foreignKey), ids)).toQuery.toList
      r.groupBy(_.getOption[Any](foreignKey).orNull)
    }

    def associate(m: T): T = companion.inTransaction {
      if (hasConstraint) delete() else remove()
      if (m.isNewRecord) m.save(throws = true)
      assignConditions(m).update
      relation.cache = List(m)
      m
    }

    def :=(m: T): T = associate(m)

    def remove(): Option[T] = companion.inTransaction {
      if (hasConstraint) {
        throw ActiveRecordException.notNullConstraint(foreignKey)
      }
      val result = toOption
      result.foreach {r =>
        r.setValue(foreignKey, None)
        r.save(throws = true)
      }
      relation.cache = Nil
      result
    }
  }

  class HasOneThroughAssociation[O <: AR, T <: AR, I <: AR](
    val owner: O, val through: SingularAssociation[O, I],
    conditions: Map[String, Any], val foreignKey: String
  )(implicit val manifest: ClassTag[T], m: ClassTag[I]) extends SingularAssociation[O, T] {
    val allConditions = conditions

    private lazy val idFieldInfo = fieldInfo("id")

    protected lazy val hasConstraint = !through.fieldInfo(foreignKey).isOption

    private def joinedRelation = source.joins[I]((m, inter) =>
      idFieldInfo.toEqualityExpression(m.id, inter.getValue[Any](foreignKey))
    ).where(condition).limit(1)

    lazy val relation2: Relation2[T, I, T] =
      joinedRelation.where((m, inter) => through.condition(inter))

    def relation: Relation[T, T] = relation2

    def eagerLoad[S <: AR](sources: List[S])
      (implicit m: ClassTag[S]): Map[Any, List[T]] = {
      val idMap = through.eagerLoad(sources).map {
        case (id, inters) => (id, inters.flatMap(_.getOption[Any](foreignKey)))
      }
      val ids = idMap.values.flatten.toList.distinct

      val recordMap = source.where(m => idFieldInfo.toInExpression(m.id, ids))
        .toList.map(m => (m.id, m)).toMap
      idMap.map {case (id, ids) => (id, ids.map(recordMap))}
    }

    def associate(m: T): I = companion.inTransaction {
      if (m.isNewRecord) throw ActiveRecordException.recordMustBeSaved
      if (hasConstraint) delete() else remove()
      assignConditions(m).update
      relation.cache = List(m)
      val inter = through.build
      through.fieldInfo(foreignKey).setValue(inter, m.id)
      inter.save(throws = true)
      through.relation.cache = List(inter)
      inter
    }

    def :=(m: T): I = associate(m)

    def remove(): Option[T] = companion.inTransaction {
      if (hasConstraint) {
        throw ActiveRecordException.notNullConstraint(foreignKey)
      }

      val result = toOption
      through.relation.foreach {r =>
        r.setValue(foreignKey, None)
        r.save(throws = true)
      }
      through.relation.cache = Nil
      relation.cache = Nil
      result
    }

    override def delete(): Option[T] = companion.inTransaction {
      val result = super.delete()
      if (hasConstraint) through.delete() else remove()
      result
    }
  }

  class HasManyAssociation[O <: AR, T <: AR](
    val owner: O, conditions: Map[String, Any], val foreignKey: String
  )(implicit val manifest: ClassTag[T]) extends CollectionAssociation[O, T] {
    val allConditions = conditions + (foreignKey -> owner.id)

    protected lazy val hasConstraint = !fieldInfo(foreignKey).isOption

    lazy val relation1: Relation1[T, T] = source.where(condition)

    def relation: Relation[T, T] = relation1

    def eagerLoad[S <: AR](sources: List[S])
      (implicit m: ClassTag[S]): Map[Any, List[T]] = {
      val ids = sources.map(_.id)
      val field = fieldInfo(foreignKey)

      val r = source.where(conditionFactory(conditions)).where(
        m => field.toInExpression(m.getValue[Any](foreignKey), ids)).toQuery.toList
      r.groupBy(_.getOption[Any](foreignKey).orNull)
    }

    def assign(m: T): T = assignConditions(m)

    def associate(m: T): T = companion.inTransaction {
      if (m.isNewRecord) m.save(throws = true)
      assign(m).update
    }

    def <<(m: T): T = associate(m)

    def <<(list: Iterable[T]): List[T] = companion.inTransaction {
      list.toList.map(associate)
    }

    def +=(m: T): T = this << m

    def ++=(list: Iterable[T]): List[T] = this << list

    def :=(list: Iterable[T]): List[T] = companion.inTransaction {
      if (hasConstraint) deleteAll() else removeAll()
      relation.cache = list.toList.map(associate)
    }

    private def remove(r: Relation[T, T]) = companion.inTransaction {
      if (hasConstraint) {
        throw ActiveRecordException.notNullConstraint(foreignKey)
      }
      val result = r.toList
      result.foreach {m =>
        m.setValue(foreignKey, None)
        m.save(throws = true)
      }
      r.cache = Nil
      result
    }

    def remove(m: T): Option[T] = {
      val field = fieldInfo("id")
      val result = remove(relation.where(r =>
        field.toEqualityExpression(r.id, m.id)
      ))
      if (relation.isLoaded) {
        relation.cache = relation.cache.filterNot(_.id == m.id)
      }
      result.headOption
    }

    def removeAll(): List[T] = remove(relation)
  }

  class HasManyThroughAssociation[O <: AR, T <: AR, I <: AR](
    val owner: O, val through: CollectionAssociation[O, I],
    conditions: Map[String, Any], val foreignKey: String
  )(implicit val manifest: ClassTag[T], m: ClassTag[I]) extends CollectionAssociation[O, T] {
    val allConditions = conditions

    private lazy val idFieldInfo = fieldInfo("id")

    protected lazy val hasConstraint = !through.fieldInfo(foreignKey).isOption

    private def joinedRelation = source.joins[I]((m, inter) =>
      idFieldInfo.toEqualityExpression(m.id, inter.getValue[Any](foreignKey))
    ).where(condition)

    lazy val relation2: Relation2[T, I, T] =
      joinedRelation.where((m, inter) => through.condition(inter))

    def relation: Relation[T, T] = relation2

    def eagerLoad[S <: AR](sources: List[S])
      (implicit m: ClassTag[S]): Map[Any, List[T]] = {
      val idMap = through.eagerLoad(sources).map {
        case (id, inters) => (id, inters.flatMap(_.getOption[Any](foreignKey)))
      }
      val ids = idMap.values.flatten.toList.distinct

      val recordMap = source.where(m => idFieldInfo.toInExpression(m.id, ids))
        .toList.map(m => (m.id, m)).toMap
      idMap.map {case (id, ids) => (id, ids.map(recordMap))}
    }

    def assign(m: T): I = {
      if (m.isNewRecord) throw ActiveRecordException.recordMustBeSaved
      assignConditions(m)
      val inter = through.build
      through.fieldInfo(foreignKey).setValue(inter, m.id)
      inter
    }

    def associate(m: T): I = assign(m).update

    def <<(m: T): I = associate(m)

    def <<(list: Iterable[T]): List[I] = companion.inTransaction {
      list.toList.map(associate)
    }

    def +=(m: T): I = this << m

    def ++=(list: Iterable[T]): List[I] = this << list

    def :=(list: Iterable[T]): List[I] = companion.inTransaction {
      if (hasConstraint) deleteAll() else removeAll()
      relation.cache = list.toList
      relation.cache.map(associate)
    }

    private def remove(r: Relation[I, I]) = companion.inTransaction {
      if (hasConstraint) {
        throw ActiveRecordException.notNullConstraint(foreignKey)
      }

      val result = r.toList
      result.foreach {m =>
        m.setValue(foreignKey, None)
        m.save(throws = true)
      }
      r.cache = Nil
      result
    }

    def remove(m: T): Option[T] = {
      val field = through.fieldInfo(foreignKey)
      val inters = remove(through.relation.where(r =>
        field.toEqualityExpression(r.getValue[Any](foreignKey), m.id)
      ))
      if (relation.isLoaded) {
        relation.cache = relation.cache.filterNot(_.id == m.id)
      }
      if (inters.nonEmpty) Some(m) else None
    }

    def removeAll(): List[T] = inTransaction {
      val result = relation.toList
      remove(through.relation)
      relation.cache = Nil
      result
    }

    override def deleteAll(): List[T] = companion.inTransaction {
      val result = super.deleteAll()
      if (hasConstraint) through.deleteAll() else removeAll()
      relation.cache = Nil
      result
    }
  }

  class HasAndBelongsToManyAssociation[O <: ActiveRecord, T <: ActiveRecord](
    val owner: O, conditions: Map[String, Any],
    interCompanion: IntermediateRecordCompanion
  )(implicit val manifest: ClassTag[T]) extends CollectionAssociation[O, T] {

    abstract sealed trait Side
    case object LeftSide extends Side
    case object RightSide extends Side

    private val (ownerSide, assocSide) = {
      val c = Seq(owner.getClass, manifest.runtimeClass).sortBy(_.getSimpleName)
      if (c.head == owner.getClass) {
        (LeftSide, RightSide)
      } else {
        (RightSide, LeftSide)
      }
    }

    lazy val foreignKey = if (assocSide == LeftSide) "leftId" else "rightId"

    protected val hasConstraint = false

    private def getId(inter: IntermediateRecord, side: Side) = {
      side match {
        case LeftSide => inter.leftId
        case RightSide => inter.rightId
      }
    }

    val allConditions = conditions

    private def joinedRelation = {
      val on = {(m: T, inter: IntermediateRecord) =>
        getId(inter, assocSide) === m.id
      }
      val select = {(m: T, inter: IntermediateRecord) => m}

      new Relation2(
        Parameters[T, (T, IntermediateRecord), T](selector = Function.tupled(select)),
        companion.table, interCompanion.table, Function.tupled(on)
      ).where(condition)
    }

    lazy val relation2: Relation2[T, IntermediateRecord, T] = {
      joinedRelation.where((m, inter) => getId(inter, ownerSide) === owner.id)
    }

    def relation: Relation[T, T] = relation2

    def eagerLoad[S <: AR](sources: List[S])
      (implicit m: ClassTag[S]): Map[Any, List[T]] = {
      val ids = sources.map(_.id).asInstanceOf[List[Long]]
      joinedRelation.where((m, inter) =>
        getId(inter, ownerSide) in ids
      ).select((m, inter) =>
        (getId(inter, ownerSide), m)
      ).toList.groupBy(_._1).view.mapValues(_.map(_._2)).toMap[Any, List[T]]
    }

    def associate(m: T): T = companion.inTransaction {
      if (m.isNewRecord) m.save(throws = true)
      val t = assignConditions(m)
      val inter = interCompanion.newInstance
      assocSide match {
        case LeftSide =>
          inter.setValue("leftId", m.id)
          inter.setValue("rightId", owner.id)
        case RightSide =>
          inter.setValue("rightId", m.id)
          inter.setValue("leftId", owner.id)
      }
      inter.save(throws = true)
      t
    }

    def <<(m: T): T = associate(m)

    def <<(list: Iterable[T]): List[T] = companion.inTransaction {
      list.toList.map(associate)
    }

    def +=(m: T): T = this << m

    def ++=(list: Iterable[T]): List[T] = this << list

    def :=(list: Iterable[T]): List[T] = inTransaction {
      interCompanion.forceDelete(inter =>
        getId(inter, ownerSide) === owner.id
      )
      relation.cache = list.toList.map(associate)
    }

    def remove(m: T): Option[T] = {
      val count = interCompanion.forceDelete(inter =>
        getId(inter, ownerSide) === owner.id and
        getId(inter, assocSide) === m.id
      )
      if (count > 0) {
        if (relation.isLoaded) {
          relation.cache = relation.cache.filterNot(_.id == m.id)
        }
        Some(m)
      } else {
        None
      }
    }

    def removeAll(): List[T] = companion.inTransaction {
      val result = relation.toList
      interCompanion.forceDelete(inter =>
        getId(inter, ownerSide) === owner.id
      )
      relation.cache = Nil
      result
    }
  }

  trait AssociationSupport { self: AR =>
    protected def belongsTo[T <: AR]
      (implicit m: ClassTag[T]): BelongsToAssociation[this.type, T] =
        belongsTo[T](Config.schema(self.recordCompanion).foreignKeyFromClass(m.runtimeClass))
          .asInstanceOf[BelongsToAssociation[this.type, T]]

    protected def belongsTo[T <: AR](foreignKey: String)
      (implicit m: ClassTag[T]): BelongsToAssociation[this.type, T] =
        new BelongsToAssociation[this.type, T](self, foreignKey)

    protected def hasOne[T <: AR]
      (implicit m: ClassTag[T]): HasOneAssociation[this.type, T] =
        hasOne[T]().asInstanceOf[HasOneAssociation[this.type, T]]

    protected def hasOne[T <: AR]
      (conditions: Map[String, Any] = Map.empty, foreignKey: String = null)
      (implicit m: ClassTag[T]): HasOneAssociation[this.type, T] = {
        val key = Option(foreignKey).getOrElse(
          Config.schema(self.recordCompanion).foreignKeyFromClass(self.getClass))
        new HasOneAssociation[this.type, T](self, conditions, key)
      }

    protected def hasOneThrough[T <: AR, I <: AR](
      through: SingularAssociation[this.type, I],
      conditions: Map[String, Any] = Map.empty,
      foreignKey: String = null
    )(implicit m1: ClassTag[T], m2: ClassTag[I]): HasOneThroughAssociation[this.type, T, I] = {
      val key = Option(foreignKey).getOrElse(
        Config.schema(self.recordCompanion).foreignKeyFromClass(m1.runtimeClass))

      new HasOneThroughAssociation[this.type, T, I](self, through, conditions, key)(m1, m2)
    }

    protected def hasMany[T <: AR]
      (implicit m: ClassTag[T]): HasManyAssociation[this.type, T] =
        hasMany[T]().asInstanceOf[HasManyAssociation[this.type, T]]

    protected def hasMany[T <: AR]
      (conditions: Map[String, Any] = Map.empty, foreignKey: String = null)
      (implicit m: ClassTag[T]): HasManyAssociation[this.type, T] = {
        val key = Option(foreignKey).getOrElse(
          Config.schema(self.recordCompanion).foreignKeyFromClass(self.getClass))
        new HasManyAssociation[this.type, T](self, conditions, key)
      }

    protected def hasManyThrough[T <: AR, I <: AR](
      through: CollectionAssociation[this.type, I],
      conditions: Map[String, Any] = Map.empty,
      foreignKey: String = null
    )(implicit m1: ClassTag[T], m2: ClassTag[I]): HasManyThroughAssociation[this.type, T, I] = {
      val key = Option(foreignKey).getOrElse(
        Config.schema(self.recordCompanion).foreignKeyFromClass(m1.runtimeClass))

      new HasManyThroughAssociation[this.type, T, I](self, through, conditions, key)(m1, m2)
    }
  }

  trait HabtmAssociationSupport { self: ActiveRecord =>
    protected def hasAndBelongsToMany[T <: ActiveRecord]
      (implicit m: ClassTag[T]): HasAndBelongsToManyAssociation[this.type, T] =
      hasAndBelongsToMany[T](Map.empty[String, Any])(m)
        .asInstanceOf[HasAndBelongsToManyAssociation[this.type, T]]

    protected def hasAndBelongsToMany[T <: ActiveRecord]
      (conditions: Map[String, Any])
      (implicit m: ClassTag[T]): HasAndBelongsToManyAssociation[this.type, T] =
    {
      val name = Config.schema(self.recordCompanion).tableNameFromClasses(self.getClass, m.runtimeClass)
      val companion = new IntermediateRecordCompanion {
        val tableName = name
      }
      new HasAndBelongsToManyAssociation[this.type, T](self, conditions, companion)
    }
  }
}

case class IntermediateRecord() extends ActiveRecordBase[CKey] with KeyedEntity[CKey] {
  val leftId: Long = 0
  val rightId: Long = 0
  def id: CKey = compositeKey(leftId, rightId)

  private[inner] var interCompanion: IntermediateRecordCompanion = _
  override lazy val _companion =
    interCompanion.asInstanceOf[ProductModelCompanion[this.type]]
}

trait IntermediateRecordCompanion extends ActiveRecordBaseCompanion[CKey, IntermediateRecord] {
  val tableName: String

  override lazy val targetClass = classOf[IntermediateRecord]
  override lazy val table = schema.getTable[IntermediateRecord](tableName)

  override def newInstance: IntermediateRecord = {
    val m = super.newInstance
    m.interCompanion = this
    m
  }
}

object IntermediateRecord {
  val keyedEntityDef = new KeyedEntityDef[IntermediateRecord, CKey] {
    def getId(m: IntermediateRecord): CKey = m.id
    def isPersisted(m: IntermediateRecord): Boolean = m.isPersisted
    val idPropertyName = "id"
  }
}
