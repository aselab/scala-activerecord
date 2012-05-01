package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import java.util.{Date, UUID}
import java.sql.Timestamp
import mojolly.inflector.InflectorImports._

/**
 * Base class of ActiveRecord objects.
 *
 * This class provides object-relational mapping and CRUD logic and callback hooks.
 */
abstract class ActiveRecord extends KeyedEntity[Long] with Product
  with CRUDable with RecordRelationSupport
{
  import ReflectionUtil._

  /** primary key */
  val id: Long = 0L

  /** corresponding ActiveRecordCompanion object */
  lazy val _companion = ReflectionUtil.classToCompanion(getClass)
    .asInstanceOf[ActiveRecordCompanion[this.type]]

  override def isNewInstance = id == 0

  override def equals(obj: Any): Boolean = obj match {
    case p: Product => productIterator.toList == p.productIterator.toList
    case ar: AnyRef => super.equals(obj)
    case _ => false
  }

  protected def doCreate = {
    _companion.create(this)
    true
  }

  protected def doUpdate = {
    _companion.update(this)
    true
  }

  protected def doDelete = _companion.delete(id)

  protected lazy val relations = _companion.schema.relations
}

/**
 * Base class of ActiveRecord companion objects.
 *
 * This class provides database table mapping and query logic.
 */
trait ActiveRecordCompanion[T <: ActiveRecord] extends ReflectionUtil {
  /** self reference */
  protected def self: this.type = this

  /** database schema */
  lazy val schema = Config.schema

  /**
   * corresponding database table
   */
  implicit lazy val table: Table[T] = {
    val name = getClass.getName.dropRight(1)
    schema.tables(name).asInstanceOf[Table[T]]
  }

  /**
   * implicit conversion for query chain.
   */
  implicit def toRichQuery(query: Queryable[T])(implicit m: Manifest[T]) =
    RichQuery(query)

  implicit def toRichQuery(t: this.type)(implicit m: Manifest[T]) =
    RichQuery(t.table)

  implicit def toRichQuery(r: ActiveRecordOneToMany[T])
    (implicit m: Manifest[T]) = RichQuery(r.relation)

  implicit def toRichQueryA[A <: KeyedEntity[_]](r: ActiveRecordManyToMany[T, A])(implicit m: Manifest[T]) = RichQuery(r.relation)

  implicit def toModelList(query: Query[T]) = query.toList
  implicit def toModelList(r: ActiveRecordOneToMany[T]) = r.toList
  implicit def toModelListA[A <: KeyedEntity[_]](r: ActiveRecordManyToMany[T, A]) = r.toList
  implicit def toModel(r: ActiveRecordManyToOne[T]) = r.one

  implicit def toQueryable(t: this.type) = t.table

  /**
   * all search.
   */
  def all = from(table)(m => select(m))

  /**
   * same as find method.
   */
  def apply(id: Long) = find(id)

  /**
   * search by id.
   */
  def find(id: Long) = inTransaction { table.lookup(id) }

  /**
   * query search.
   *
   * {{{
   * where {m: T => m.name === "abc" and m.age.~ > 20}
   * }}}
   *
   * @param condition search condition
   * @param query table or subquery in from clause. default is table
   */
  def where(condition: (T) => org.squeryl.dsl.ast.LogicalBoolean)(implicit query: Queryable[T]): Query[T] = {
    from(query)(m => PrimitiveTypeMode.where(condition(m)) select(m))
  }

  /**
   * Search by multiple fieldnames and values and return first record.
   *
   * {{{
   * findBy("name" -> "abc", "email" -> "abc@foo.bar")
   * }}}
   * @param condition fieldname-value tuple
   * @param conditions multiple fieldname-value tuples(optional)
   * @param query table or subquery in from clause. default is table
   */
  def findBy(condition: (String, Any), conditions: (String, Any)*)(implicit query: Queryable[T]): Option[T] = findAllBy(condition, conditions:_*).headOption

  /**
   * Search by multiple fieldnames and values.
   *
   * {{{
   * findAllBy("name" -> "abc", "email" -> "abc@foo.bar")
   * }}}
   * @param condition fieldname-value tuple
   * @param conditions multiple fieldname-value tuples(optional)
   * @param query table or subquery in from clause. default is table
   */
  def findAllBy(condition: (String, Any), conditions: (String, Any)*)(implicit query: Queryable[T]): Query[T] = {
    conditions.foldLeft(findAllBy(condition._1, condition._2)(query)) {
      case (subquery, cond) => findAllBy(cond._1, cond._2)(subquery)
    }
  }

  /**
   * Search by fieldname and value and return first record.
   * {{{
   * findBy("name", "abc")
   * }}}
   * @param name field name
   * @param value field value
   * @param query table or subquery in from clause. default is table
   */
  def findBy(name: String, value: Any)(implicit query: Queryable[T]):Option[T] =
    findAllBy(name, value).headOption

  /**
   * Search by fieldname and value.
   * {{{
   * findAllBy("name", "abc")
   * }}}
   * @param name field name
   * @param value field value
   * @param query table or subquery in from clause. default is table
   */
  def findAllBy(name: String, value: Any)(implicit query: Queryable[T]): Query[T] = {
    where(value match {
      case v: String => {m: T => m.getValue[String](name) === v}
      case Some(v: String) => {m: T => m.getValue[Option[String]](name) === Some(v)}
      case v: Boolean => {m: T => m.getValue[Boolean](name) === v}
      case Some(v: Boolean) => {m: T => m.getValue[Option[Boolean]](name) === Some(v)}
      case v: Int => {m: T => m.getValue[Int](name) === v}
      case Some(v: Int) => {m: T => m.getValue[Option[Int]](name) === Some(v)}
      case v: Long => {m: T => m.getValue[Long](name) === v}
      case Some(v: Long) => {m: T => m.getValue[Option[Long]](name) === Some(v)}
      case v: Float => {m: T => m.getValue[Float](name) === v}
      case Some(v: Float) => {m: T => m.getValue[Option[Float]](name) === Some(v)}
      case v: Double => {m: T => m.getValue[Double](name) === v}
      case Some(v: Double) => {m: T => m.getValue[Option[Double]](name) === Some(v)}
      case v: BigDecimal => {m: T => m.getValue[BigDecimal](name) === v}
      case Some(v: BigDecimal) => {m: T => m.getValue[Option[BigDecimal]](name) === Some(v)}
      case v: Timestamp => {m: T => m.getValue[Timestamp](name) === v}
      case Some(v: Timestamp) => {m: T => m.getValue[Option[Timestamp]](name) === Some(v)}
      case v: Date => {m: T => m.getValue[Date](name) === v}
      case Some(v: Date) => {m: T => m.getValue[Option[Date]](name) === Some(v)}
      case v: UUID => {m: T => m.getValue[UUID](name) === v}
      case Some(v: UUID) => {m: T => m.getValue[Option[UUID]](name) === Some(v)}
      case _ => ActiveRecordException.unsupportedType(
        "%s by %s".format(name, value.toString))
    })(query)
  }

  /**
   * insert record from model.
   */
  def create(model: T) = inTransaction {
    table.insert(model)
  }

  /**
   * update record from model.
   */
  def update(model: T) = inTransaction {
    table.update(model)
  }

  /**
   * delete record from id.
   */
  def delete(id: Long) = inTransaction {
    table.delete(id)
  }

  /**
   * delete all records.
   */
  def deleteAll() = inTransaction {
    table.delete(all)
  }

  /**
   * unique validation.
   */
  def isUnique(name: String, m: T): Boolean = inTransaction {
    val newValue = m.getValue[Any](name)

    if (newValue == null || newValue == None) return true

    val result = findBy(name, newValue)
    find(m.id) match {
      case Some(old) if old.getValue[Any](name) != newValue => result.isEmpty
      case Some(_) => true
      case None => result.isEmpty
    }
  }

  /** Unique annotated fields */
  lazy val uniqueFields =
    formatFields.filter(_.isAnnotationPresent(classOf[annotations.Unique]))

  /** corresponding ActiveRecord class */
  private val targetClass = companionToClass(this)

  /**
   * Create a new model object.
   * ActiveRecord class must implement default constructor.
   */
  def newInstance = try {
    targetClass.newInstance.asInstanceOf[T]
  } catch {
    case e: InstantiationException =>
      ActiveRecordException.defaultConstructorRequired
  }

  /** ActiveRecord fields information */
  lazy val fieldInfo = {
    val m = newInstance
    formatFields.map { f =>
      val name = f.getName
      (name, FieldInfo(f, m.getValue[Any](name)))
    }.toMap
  }

  lazy val formatFields: List[java.lang.reflect.Field] =
    targetClass.getDeclaredFields.filterNot {f =>
      f.isAnnotationPresent(classOf[annotations.Ignore]) ||
      f.getName.contains("$")
    }.toList

}

case class RichQuery[T <: ActiveRecord](query: Queryable[T])(implicit m: Manifest[T]) {
  import org.squeryl.dsl.ast._

  val companion = ReflectionUtil.classToCompanion(m.erasure)
    .asInstanceOf[ActiveRecordCompanion[T]]

  def where(condition: (T) => LogicalBoolean): Query[T] =
    companion.where(condition)(query)

  def findBy(condition: (String, Any), conditions: (String, Any)*): Option[T] =
    findAllBy(condition, conditions:_*).headOption

  def findAllBy(condition: (String, Any), conditions: (String, Any)*):Query[T] =
    companion.findAllBy(condition, conditions:_*)(query)

  def findBy(name: String, value: Any): Option[T] =
    findAllBy(name, value).headOption

  def findAllBy(name: String, value: Any): Query[T] =
    companion.findAllBy(name, value)(query)

  /**
   * sort results.
   *
   * {{{
   * Person.findAllBy("country", "Japan").orderBy(p => p.age asc)
   * Person.all.orderBy(p => p.age asc, p => p.name asc)
   * }}}
   * @param condition sort condition
   * @param conditions multiple sort conditions(optional)
   */
  def orderBy(condition: (T) => OrderByExpression, conditions: (T => OrderByExpression)*) =
    from(query)(m => select(m).orderBy((condition :: conditions.toList).map(_.apply(m))))

  /**
   * returns limited results.
   * {{{
   * Post.all.orderBy(p => p.updatedAt desc).limit(10)
   * }}}
   * @param count max count
   */
  def limit(count: Int) = page(0, count)

  /**
   * returns page results.
   * {{{
   * Post.all.orderBy(p => p.updatedAt desc).page(10 * (pageNumber - 1), 10)
   * }}}
   * @param offset offset count
   * @param count max count
   */
  def page(offset: Int, count: Int) = query match {
    case _: Query[_] => query.asInstanceOf[Query[T]].page(offset, count)
    case _ => from(query)(m => select(m)).page(offset, count)
  }

  def count: Long = from(query)(m => compute(PrimitiveTypeMode.count))
}

/**
 * Base class of database schema.
 */
trait ActiveRecordTables extends Schema with TableRelationSupport {
  import ReflectionUtil._

  lazy val tables = {
    val exceptType = classOf[ManyToManyRelationImpl[_, _, _]]
    this.getFields[Table[AR]].collect {
      case f if !exceptType.isAssignableFrom(f.getType) =>
        val name = getGenericType(f).getName
        (name, this.getValue[Table[AR]](f.getName))
    }.toMap
  }

  /** All tables */
  lazy val all = tables.values

  override def tableNameFromClass(c: Class[_]) = super.tableNameFromClass(c).pluralize

  private lazy val createTables = inTransaction {
    val isCreated = all.headOption.exists{ t =>
      try {
        t.lookup(1L)
        true
      } catch {
        case e => false
      }
    }

    if (!isCreated) create
  }

  private var _initialized = false

  /** load configuration and then setup database and session */
  def initialize(implicit config: Map[String, Any] = Map()) {
    if (_initialized)
      return
    Config.conf = loadConfig(config)

    // declare id field on all tables
    all.foreach(on(_)(t => declare(
      t.id is(primaryKey, autoIncremented)
    )))

    SessionFactory.concreteFactory = Some(() => session)

    createTables
    _initialized = true
  }

  /** cleanup database resources */
  def cleanup = Config.cleanup

  def loadConfig(config: Map[String, Any]): ActiveRecordConfig =
    DefaultConfig(overrideSettings = config)

  def session = Session.create(Config.connection, Config.adapter)

  /** drop and create table */
  def reset = inTransaction {
    drop
    create
  }
}

object Config {
  var conf: ActiveRecordConfig = _

  lazy val schema = ReflectionUtil.classToCompanion(conf.schemaClass)
    .asInstanceOf[ActiveRecordTables]

  def connection = conf.connection
  def adapter = conf.adapter

  def cleanup = conf.cleanup
}

