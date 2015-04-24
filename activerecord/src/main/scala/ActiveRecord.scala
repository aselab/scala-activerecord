package com.github.aselab.activerecord

import com.github.aselab.activerecord.dsl._
import inner._

trait ActiveModel extends ProductModel
  with io.FormSerializer
  with io.JsonSerializer
  with validations.ValidationSupport
{
  def isNewRecord: Boolean = true
}

trait ActiveModelCompanion[T <: ActiveModel] extends ProductModelCompanion[T]
  with io.FormSupport[T]
  with io.JsonSupport[T]
{
  override def newInstance: T = super.newInstance

  import scala.language.experimental.macros
  def newInstance(data: (String, Any)*): T = macro MethodMacros.newInstance[T]
}

trait ActiveRecordBase[T] extends CRUDable with ActiveModel with ActiveRecord.AssociationSupport
{
  def id: T
  def isPersisted: Boolean

  private var _isDeleted: Boolean = false
  def isDeleted: Boolean = _isDeleted

  /** corresponding ActiveRecordCompanion object */
  lazy val recordCompanion = _companion.asInstanceOf[ActiveRecordBaseCompanion[T, this.type]]

  override def isNewRecord: Boolean = !isPersisted

  override def save(): Boolean = save(false)

  def save(throws: Boolean = false, validate: Boolean = true): Boolean = {
    val result = if (validate) super.save else super.saveWithoutValidation
    result || (throws && (throw ActiveRecordException.saveFailed(errors)))
  }

  def create: this.type = create(true)
  def create(validate: Boolean): this.type = {
    if (isNewRecord) {
      save(true, validate)
      this
    } else {
      throw ActiveRecordException.notImplemented
    }
  }

  def update: this.type = update(true)
  def update(validate: Boolean): this.type = {
    save(true, validate)
    this
  }

  def reload: Option[this.type] = {
    recordCompanion.find(id)
  }

  protected def doCreate: Boolean = {
    recordCompanion.create(this)
    true
  }

  protected def doUpdate: Boolean = {
    recordCompanion.update(this)
    true
  }

  protected def doDelete: Boolean = {
    val result = if (isDeleted) false else recordCompanion.delete(id)
    if (result) _isDeleted = true
    result
  }

  override def toMap: Map[String, Any] = if (isNewRecord) {
    super.toMap - "id"
  } else {
    super.toMap
  }

  override def unsafeAssign(data: Map[String, Any]): this.type = super.unsafeAssign(data - "id")

  def recordInDatabase: Option[this.type] = recordCompanion.find(id)
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

  def isPersisted: Boolean = id > 0 && !isDeleted
}

object ActiveRecord extends inner.Relations with inner.Associations

trait ActiveRecordBaseCompanion[K, T <: ActiveRecordBase[K]]
  extends ActiveModelCompanion[T] with inner.CompanionConversion[T] {
  import reflections.ReflectionUtil.toReflectable
  import ActiveRecord.Relation1
  import scala.reflect.ClassTag

  implicit val manifest: ClassTag[T] = ClassTag(targetClass)

  lazy val isOptimistic =
    classOf[Optimistic].isAssignableFrom(manifest.runtimeClass)

  implicit val keyedEntityDef = new KeyedEntityDef[T, K] {
    def getId(m: T): K = m.id
    def isPersisted(m: T): Boolean = m.isPersisted
    val idPropertyName = "id"
    override def optimisticCounterPropertyName: Option[String] =
      if (isOptimistic) Some("occVersionNumber") else None
  }

  /** self reference */
  protected def self: this.type = this

  /** database schema */
  lazy val schema = Config.schema(this)

  def defaultScope: Relation1[T, T] = queryToRelation[T](table)

  /**
   * corresponding database table
   */
  lazy val table: Table[T] = {
    val name = getClass.getName.dropRight(1)
    schema.getTable(name)
  }

  def inTransaction[R](f: => R): R = schema.inTransaction(f)

  def transaction[R](f: => R): R = schema.transaction(f)

  /**
   * all search.
   */
  def all: Relation1[T, T] = defaultScope

  /**
   * search by id.
   */
  def find(id: K): Option[T] = inTransaction { table.lookup(id) }

  /**
   * insert record from model.
   */
  protected[activerecord] def create(model: T): Unit = inTransaction {
    table.insert(model)
  }

  /**
   * update record from model.
   */
  protected[activerecord] def update(model: T): Unit = inTransaction {
    table.update(model)
  }

  /**
   * delete record from id.
   */
  protected[activerecord] def delete(id: K): Boolean = inTransaction {
    table.delete(id)
  }

  def forceUpdateAll(updateAssignments: (T => UpdateAssignment)*): Int =
    all.forceUpdateAll(updateAssignments: _*)

  def forceUpdate(condition: T => LogicalBoolean)(updateAssignments: (T => UpdateAssignment)*): Int =
    all.where(condition).forceUpdateAll(updateAssignments: _*)

  def forceDeleteAll(): Int = inTransaction {
    table.delete(all.toQuery)
  }

  def forceDelete(condition: T => LogicalBoolean): Int = inTransaction {
    table.deleteWhere(m => condition(m))
  }

  def forceInsertAll(models: Iterable[T]): Unit = inTransaction {
    table.insert(models)
  }

  def forceInsertAll(models: T*): Unit = forceInsertAll(models.toIterable)

  def insertWithValidation(models: Iterable[T]): Iterable[T] = {
    val (valid, invalid) = models.partition(_.validate)
    forceInsertAll(valid)
    invalid
  }

  def insertWithValidation(models: T*): Iterable[T] = insertWithValidation(models.toIterable)

  /**
   * unique validation.
   */
  def isUnique(name: String, m: T): Boolean = m.getValue[Any](name) match {
    case value if value == null || value == None =>
      true
    case value => inTransaction {
      find(m.id) match {
        case Some(old) if old.getValue[Any](name) != value =>
          all.unsafeFindBy((name, value)).isEmpty
        case Some(_) => true
        case None => all.unsafeFindBy((name, value)).isEmpty
      }
    }
  }

  /** Unique annotated fields */
  lazy val uniqueFields = fields.filter(_.isUnique)

  def fromMap(data: Map[String, Any]): T = newInstance.unsafeAssign(data)
}

/**
 * Base class of ActiveRecord companion objects.
 *
 * This class provides database table mapping and query logic.
 */
trait ActiveRecordCompanion[T <: ActiveRecord] extends ActiveRecordBaseCompanion[Long, T]

