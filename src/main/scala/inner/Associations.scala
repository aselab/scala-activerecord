package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import org.squeryl._
import org.squeryl.dsl.ast.{LogicalBoolean, EqualityExpression}
import squeryl.Implicits._
import ReflectionUtil._

trait Associations {
  trait Association[O <: AR, T <: AR] {
    val owner: O
    val associationClass = manifest.erasure
    implicit val manifest: Manifest[T]

    def relation: ActiveRecord.Relation[T, T]

    protected lazy val companion = classToCompanion(associationClass)
      .asInstanceOf[ActiveRecordBaseCompanion[_, T]]

    protected lazy val source =
      ActiveRecord.Relation(companion.table, {m: T => m})(manifest)

    protected[inner] def fieldInfo(name: String) = companion.fieldInfo.getOrElse(name,
      throw ActiveRecordException.notFoundField(name)) 
  }

  trait CollectionAssociation[O <: AR, T <: AR] extends Association[O, T] {
    
    val allConditions: Map[String, Any]

    def condition: T => LogicalBoolean = {
      m => LogicalBoolean.and(allConditions.map {
        case (key, value) =>
          fieldInfo(key).toEqualityExpression(m.getValue(key), value)
      }.toSeq)
    }

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
      result
    }
  }

  class BelongsToAssociation[O <: AR, T <: AR](
    val owner: O, foreignKey: String
  )(implicit val manifest: Manifest[T]) extends Association[O, T] {
    lazy val fieldInfo = owner._companion.fieldInfo(foreignKey)

    def condition: T => LogicalBoolean = {
      m => fieldInfo.toEqualityExpression(m.id, owner.getValue(foreignKey))
    }

    def relation1: ActiveRecord.Relation1[T, T] = source.where(condition).limit(1)

    def relation = relation1

    def toOption: Option[T] = relation.headOption

    def assign(m: T): T = {
      fieldInfo.setValue(owner, m.id)
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
    val owner: O, conditions: Map[String, Any], foreignKey: String
  )(implicit val manifest: Manifest[T]) extends CollectionAssociation[O, T] {
    val allConditions = conditions + (foreignKey -> owner.id)

    def relation1: ActiveRecord.Relation1[T, T] = source.where(condition)

    def relation = relation1

    def assign(m: T): T = assignConditions(m)

    def associate(m: T): T = {
      val t = assign(m)
      t.save
      t
    }

    def <<(m: T): T = associate(m)

    def :=(list: List[T]): List[T] = inTransaction {
      deleteAll
      list.map(associate)
    }
  }

  class HasManyThroughAssociation[O <: AR, T <: AR, I <: AR](
    val owner: O, val through: CollectionAssociation[O, I],
    conditions: Map[String, Any], foreignKey: String
  )(implicit val manifest: Manifest[T], m: Manifest[I]) extends CollectionAssociation[O, T] {
    val allConditions = conditions

    def relation2: ActiveRecord.Relation2[T, I, T] = source.joins[I]{
      (m, inter) =>
        val f = fieldInfo("id")
        val e1 = f.toExpression(m.id)
        val e2 = f.toExpression(inter.getValue(foreignKey))
        new EqualityExpression(e1, e2)
    }.where(
      (m, inter) =>
      LogicalBoolean.and(through.condition(inter) :: conditions.map {
        case (key, value) =>
          fieldInfo(key).toEqualityExpression(m.getValue(key), value)
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

    def relation2: ActiveRecord.Relation2[T, IntermediateRecord, T] = {
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
              fieldInfo(key).toEqualityExpression(m.getValue(key), value)
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
      list.map(associate)
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
