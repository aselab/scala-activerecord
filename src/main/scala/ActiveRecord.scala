package com.github.aselab.activerecord

import com.github.aselab.activerecord.dsl._

trait ActiveRecordBase[T] extends ProductModel with CRUDable
  with ActiveRecord.AssociationSupport with ValidationSupport with IO
{
  def id: T
  def isPersisted: Boolean

  /** corresponding ActiveRecordCompanion object */
  lazy val recordCompanion = _companion.asInstanceOf[ActiveRecordBaseCompanion[T, this.type]]

  override def isNewRecord: Boolean = !isPersisted

  override def save(): Boolean = save(false)

  def save(throws: Boolean): Boolean =
    super.save || (throws && (throw ActiveRecordException.saveFailed(errors)))

  protected def doCreate = {
    recordCompanion.create(this)
    true
  }

  protected def doUpdate = {
    recordCompanion.update(this)
    true
  }

  protected def doDelete = recordCompanion.delete(id)

  override def toMap: Map[String, Any] = if (isNewRecord) {
    super.toMap - "id"
  } else {
    super.toMap
  }
}

/**
 * Base class of ActiveRecord objects.
 *
 * This class provides object-relational mapping and CRUD logic and callback hooks.
 */
abstract class ActiveRecord extends ActiveRecordBase[Long]
  with ActiveRecord.HabtmAssociationSupport
{
  /** primary key */
  val id: Long = 0L

  def isPersisted: Boolean = id > 0
}

object ActiveRecord extends inner.Relations with inner.Associations

trait ActiveRecordBaseCompanion[K, T <: ActiveRecordBase[K]] extends ProductModelCompanion[T] with inner.CompanionConversion[T] with FormSupport[T] {
  import ReflectionUtil._
  import ActiveRecord._

  implicit val manifest: Manifest[T] = Manifest.classType(targetClass)

  implicit val keyedEntityDef = new KeyedEntityDef[T, K] {
    def getId(m: T) = m.id
    def isPersisted(m: T) = m.isPersisted
    def idPropertyName = "id"
  }

  /** self reference */
  protected def self: this.type = this

  /** database schema */
  lazy val schema = Config.schema

  /**
   * corresponding database table
   */
  lazy val table: Table[T] = {
    val name = getClass.getName.dropRight(1)
    schema.tableMap(name).asInstanceOf[Table[T]]
  }

  /**
   * all search.
   */
  def all: Relation1[T, T] = queryToRelation[T](table)

  /**
   * search by id.
   */
  def find(id: K): Option[T] = inTransaction { table.lookup(id) }

  /**
   * insert record from model.
   */
  protected[activerecord] def create(model: T) = inTransaction {
    table.insert(model)
  }

  /**
   * update record from model.
   */
  protected[activerecord] def update(model: T) = inTransaction {
    table.update(model)
  }

  /**
   * delete record from id.
   */
  protected[activerecord] def delete(id: K) = inTransaction {
    table.delete(id)
  }

  def forceUpdateAll(updateAssignments: (T => UpdateAssignment)*): Int = inTransaction {
    dsl.update(table)(m => setAll(updateAssignments.map(_.apply(m)): _*))
  }

  def forceUpdate(condition: T => LogicalBoolean, updateAssignments: (T => UpdateAssignment)*): Int = inTransaction {
    dsl.update(table)(m => where(condition(m)).set(updateAssignments.map(_.apply(m)): _*))
  }

  def forceDeleteAll(): Int = inTransaction {
    table.delete(all.toQuery)
  }

  def forceDelete(condition: T => LogicalBoolean): Int = inTransaction {
    table.deleteWhere(m => condition(m))
  }

  /**
   * unique validation.
   */
  def isUnique(name: String, m: T): Boolean = m.getValue[Any](name) match {
    case value if value == null || value == None =>
      true
    case value => inTransaction {
      find(m.id) match {
        case Some(old) if old.getValue[Any](name) != value =>
          all.findBy(name, value).isEmpty
        case Some(_) => true
        case None => all.findBy(name, value).isEmpty
      }
    }
  }

  /** Unique annotated fields */
  lazy val uniqueFields = fields.filter(_.isUnique)

  def fromMap(data: Map[String, Any]) {
    newInstance.assign(data)
  }
}

/**
 * Base class of ActiveRecord companion objects.
 *
 * This class provides database table mapping and query logic.
 */
trait ActiveRecordCompanion[T <: ActiveRecord] extends ActiveRecordBaseCompanion[Long, T] {
}

