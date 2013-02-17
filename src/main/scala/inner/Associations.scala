package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import squeryl.Implicits._
import ReflectionUtil._

trait Associations {
  trait Association[+O <: AR, T <: AR] {
    val owner: O
    val associationClass = manifest.erasure
    implicit val manifest: Manifest[T]

    def relation: ActiveRecord.Relation[T, T]

    protected[inner] def eagerLoad[S <: AR](sources: List[S])
      (implicit m: Manifest[S]): Map[Any, List[T]] = {
      throw new Exception("not implemented")
    }

    protected lazy val companion = classToCompanion(associationClass)
      .asInstanceOf[ActiveRecordBaseCompanion[_, T]]

    protected lazy val source =
      ActiveRecord.Relation(companion.table, {m: T => m})(manifest)

    protected[inner] def fieldInfo(name: String) =
      companion.fieldInfo.getOrElse(name, throw ActiveRecordException.notFoundField(name))
  }

  trait CollectionAssociation[O <: AR, T <: AR] extends Association[O, T] {
    
    val allConditions: Map[String, Any]

    def conditionFactory(conditions: Map[String, Any]) = {
      m: T => LogicalBoolean.and(conditions.map {
        case (key, value) =>
          fieldInfo(key).toEqualityExpression(m.getValue[Any](key), value)
      }.toSeq)
    }

    def condition: T => LogicalBoolean = conditionFactory(allConditions)

    def build: T = assignConditions(companion.newInstance)

    protected def assignConditions(m: T): T = {
      allConditions.foreach {
        case (key, value) => fieldInfo(key).setValue(m, value)
      }
      m
    }

    def deleteAll(): List[T] = inTransaction {
      val result = relation.toList
      result.foreach(_.delete)
      relation.cache = Nil
      result
    }
  }

  class BelongsToAssociation[O <: AR, T <: AR](
    val owner: O, val foreignKey: String
  )(implicit val manifest: Manifest[T]) extends Association[O, T] {
    lazy val foreignKeyInfo = owner._companion.fieldInfo(foreignKey)

    def condition: T => LogicalBoolean = {
      m => foreignKeyInfo.toEqualityExpression(m.id, owner.getValue[Any](foreignKey))
    }

    override def eagerLoad[S <: AR](sources: List[S])
      (implicit m: Manifest[S]): Map[Any, List[T]] = {
      val ids = sources.map(_.id)
      val field = foreignKeyInfo
      val r = source.joins[S]((m, o) =>
        field.toEqualityExpression(m.id, o.getValue[Any](foreignKey))
      ).where((m, o) => field.toInExpression(o.id, ids)).toQuery.toList
      val map = r.groupBy(_.id)
      sources.map(r => (r.id, map.getOrElse(r.getValue[Any](foreignKey).toOption[Any].orNull, Nil))).toMap
    }

    lazy val relation1: ActiveRecord.Relation1[T, T] =
      source.where(condition).limit(1)

    def relation = relation1

    def toOption: Option[T] = relation.headOption

    def assign(m: T): T = {
      foreignKeyInfo.setValue(owner, m.id)
      m
    }

    def associate(m: T): T = {
      val t = assign(m)
      t.save
      t
    }

    def :=(m: T): T = assign(m)
  }

  class HasManyAssociation[O <: AR, T <: AR](
    val owner: O, conditions: Map[String, Any], val foreignKey: String
  )(implicit val manifest: Manifest[T]) extends CollectionAssociation[O, T] {
    val allConditions = conditions + (foreignKey -> owner.id)

    lazy val relation1: ActiveRecord.Relation1[T, T] = source.where(condition)

    def relation = relation1

    override def eagerLoad[S <: AR](sources: List[S])
      (implicit m: Manifest[S]): Map[Any, List[T]] = {
      val ids = sources.map(_.id)
      val field = fieldInfo(foreignKey)

      val r = source.where(conditionFactory(conditions)).where(
        m => field.toInExpression(m.getValue(foreignKey), ids)).toQuery.toList
      r.groupBy(_.getValue[Any](foreignKey).toOption[Any].orNull)
    }

    def assign(m: T): T = assignConditions(m)

    def associate(m: T): T = {
      val t = assign(m)
      t.save
      t
    }

    def <<(m: T): T = associate(m)

    def :=(list: List[T]): List[T] = inTransaction {
      deleteAll
      relation.cache = list.map(associate)
    }
  }

  class HasManyThroughAssociation[O <: AR, T <: AR, I <: AR](
    val owner: O, val through: CollectionAssociation[O, I],
    conditions: Map[String, Any], foreignKey: String
  )(implicit val manifest: Manifest[T], m: Manifest[I]) extends CollectionAssociation[O, T] {
    val allConditions = conditions

    lazy val relation2: ActiveRecord.Relation2[T, I, T] = source.joins[I]{
      (m, inter) =>
        val f = fieldInfo("id")
        f.toEqualityExpression(m.id, inter.getValue[Any](foreignKey))
    }.where(
      (m, inter) =>
      LogicalBoolean.and(through.condition(inter) :: conditions.map {
        case (key, value) =>
          fieldInfo(key).toEqualityExpression(m.getValue[Any](key), value)
      }.toList)
    )

    def relation = relation2

    def assign(m: T): I = {
      assignConditions(m)
      val inter = through.build
      through.fieldInfo(foreignKey).setValue(inter, m.id)
      inter
    }

    def associate(m: T): I = {
      val i = assign(m)
      i.save
      i
    }

    def <<(m: T): I = associate(m)

    def :=(list: List[T]): List[I] = inTransaction {
      deleteAll
      relation.cache = list
      list.map(associate)
    }

    override def deleteAll(): List[T] = inTransaction {
      through.deleteAll
      super.deleteAll
    }
  }

  class HasAndBelongsToManyAssociation[O <: ActiveRecord, T <: ActiveRecord](
    val owner: O, conditions: Map[String, Any],
    interCompanion: IntermediateRecordCompanion
  )(implicit val manifest: Manifest[T]) extends CollectionAssociation[O, T] {

    private val isLeftSide = List(owner.getClass, manifest.erasure)
      .sortBy(_.getSimpleName).head == manifest.erasure

    val allConditions = conditions

    lazy val relation2: ActiveRecord.Relation2[T, IntermediateRecord, T] = {
      val on = {(m: T, inter: IntermediateRecord) =>
        m.id === (if (isLeftSide) inter.leftId else inter.rightId)
      }
      val select = {(m: T, inter: IntermediateRecord) => m}

      new ActiveRecord.Relation2(Nil, Nil, None, companion.table,
        interCompanion.table, Function.tupled(on), Function.tupled(select)
      ).where((m, inter) =>
        LogicalBoolean.and(
          (owner.id === (if (isLeftSide) inter.rightId else inter.leftId)) ::
          conditions.map {
            case (key, value) =>
              fieldInfo(key).toEqualityExpression(m.getValue[Any](key), value)
          }.toList
        )
      )
    }

    def relation = relation2

    def associate(m: T): T = {
      val t = assignConditions(m)
      val inter = interCompanion.newInstance
      if (isLeftSide) {
        inter.setValue("leftId", m.id)
        inter.setValue("rightId", owner.id)
      } else {
        inter.setValue("rightId", m.id)
        inter.setValue("leftId", owner.id)
      }
      inter.save
      t
    }

    def <<(m: T): T = associate(m)

    def :=(list: List[T]): List[T] = inTransaction {
      deleteAll
      relation.cache = list.map(associate)
    }

    override def deleteAll(): List[T] = inTransaction {
      val result = interCompanion.all.where(inter =>
        owner.id === (if (isLeftSide) inter.leftId else inter.rightId)
      ).foreach(_.delete)
      relation.toList
    }
  }


  trait AssociationSupport { self: AR =>
    protected def belongsTo[T <: AR]
      (implicit m: Manifest[T]): BelongsToAssociation[this.type, T] =
        belongsTo[T](Config.schema.foreignKeyFromClass(m.erasure))
          .asInstanceOf[BelongsToAssociation[this.type, T]]

    protected def belongsTo[T <: AR](foreignKey: String)
      (implicit m: Manifest[T]): BelongsToAssociation[this.type, T] =
        new BelongsToAssociation[this.type, T](self, foreignKey)

    protected def hasMany[T <: AR]
      (implicit m: Manifest[T]): HasManyAssociation[this.type, T] =
        hasMany[T]().asInstanceOf[HasManyAssociation[this.type, T]]

    protected def hasMany[T <: AR]
      (conditions: Map[String, Any] = Map.empty, foreignKey: String = null)
      (implicit m: Manifest[T]): HasManyAssociation[this.type, T] = {
        val key = Option(foreignKey).getOrElse(
          Config.schema.foreignKeyFromClass(self.getClass))
        new HasManyAssociation[this.type, T](self, conditions, key)
      }

    protected def hasManyThrough[T <: AR, I <: AR](
      through: CollectionAssociation[this.type, I],
      conditions: Map[String, Any] = Map.empty,
      foreignKey: String = null
    )(implicit m1: Manifest[T], m2: Manifest[I]): HasManyThroughAssociation[this.type, T, I] = {
      val key = Option(foreignKey).getOrElse(
        Config.schema.foreignKeyFromClass(m1.erasure))

      new HasManyThroughAssociation[this.type, T, I](self, through, conditions, key)(m1, m2)
    }
  }

  trait HabtmAssociationSupport { self: ActiveRecord =>
    protected def hasAndBelongsToMany[T <: ActiveRecord]
      (implicit m: Manifest[T]): HasAndBelongsToManyAssociation[this.type, T] =
      hasAndBelongsToMany[T](Map.empty[String, Any])(m)
        .asInstanceOf[HasAndBelongsToManyAssociation[this.type, T]]

    protected def hasAndBelongsToMany[T <: ActiveRecord]
      (conditions: Map[String, Any])
      (implicit m: Manifest[T]): HasAndBelongsToManyAssociation[this.type, T] =
    {
      val name = Config.schema.tableNameFromClasses(self.getClass, m.erasure)
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
  override lazy val table =
    schema.tableMap(tableName).asInstanceOf[Table[IntermediateRecord]]

  override def newInstance = {
    val m = super.newInstance
    m.interCompanion = this
    m
  }
}

object IntermediateRecord {
  val keyedEntityDef = new KeyedEntityDef[IntermediateRecord, CKey] {
    def getId(m: IntermediateRecord) = m.id
    def isPersisted(m: IntermediateRecord) = m.isPersisted
    def idPropertyName = "id"
  }
}
