package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import java.util.{Date, UUID}
import java.sql.Timestamp
import java.lang.annotation.Annotation
import mojolly.inflector.InflectorImports._

trait ProductModel extends Product {
  @dsl.Ignore
  lazy val _companion = ReflectionUtil.classToCompanion(getClass)
    .asInstanceOf[ProductModelCompanion[this.type]]
}

trait ProductModelCompanion[T <: ProductModel] {
  import ReflectionUtil._

  /** corresponding model class */
  protected val targetClass = companionToClass(this).asInstanceOf[Class[T]]

  /**
   * Create a new model object.
   */
  def newInstance = classInfo.factory.apply

  /** ProductModel class information */
  lazy val classInfo: ClassInfo[T] = ClassInfo(targetClass)

  /** ProductModel fields information */
  lazy val fieldInfo: Map[String, FieldInfo] = {
    val m = newInstance
    formatFields.map { f =>
      val name = f.getName
      (name, FieldInfo(f, m.getValue[Any](name)))
    }.toMap
  }

  lazy val formatFields: List[java.lang.reflect.Field] =
    targetClass.getDeclaredFields.filterNot {f =>
      f.isAnnotationPresent(classOf[annotations.Ignore]) ||
      classOf[RecordRelation].isAssignableFrom(f.getType) ||
      f.getName.contains("$")
    }.toList

  lazy val validators: Map[String, Seq[(Annotation, Validator[_])]] = fieldInfo.map {
    case (name, info) => (name, info.annotations.flatMap { a =>
      ValidatorFactory.get(a.annotationType).map(a -> _)
    })
  }.toMap
}

trait ActiveRecordBase[T] extends ProductModel with KeyedEntity[T]
  with CRUDable with ValidationSupport
  with ActiveRecordBaseRelationSupport with IO
{
  /** corresponding ActiveRecordCompanion object */
  lazy val recordCompanion = _companion.asInstanceOf[ActiveRecordBaseCompanion[T, this.type]]

  override def equals(obj: Any): Boolean = obj match {
    case p: Product => productIterator.toList == p.productIterator.toList
    case ar: AnyRef => super.equals(obj)
    case _ => false
  }

  override def isNewInstance = !isPersisted

  protected def doCreate = {
    recordCompanion.create(this)
    true
  }

  protected def doUpdate = {
    recordCompanion.update(this)
    true
  }

  protected def doDelete = recordCompanion.delete(id)

  protected lazy val relations: Map[(String, String), RelationWrapper[ActiveRecord, ActiveRecordBase[_]]] = recordCompanion.schema.relations
}

/**
 * Base class of ActiveRecord objects.
 *
 * This class provides object-relational mapping and CRUD logic and callback hooks.
 */
abstract class ActiveRecord extends ActiveRecordBase[Long]
  with ActiveRecordRelationSupport
{
  /** primary key */
  val id: Long = 0L

  override def isNewInstance = id == 0
}

trait ActiveRecordBaseCompanion[K, T <: ActiveRecordBase[K]] extends ProductModelCompanion[T] {
  import ReflectionUtil._

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

  implicit def toModelList(query: Query[T]) = query.toList
  implicit def toModel[A <: ActiveRecord](r: ActiveRecordManyToOne[A]) = r.one

  implicit def toQueryable(t: this.type) = t.table

  /**
   * all search.
   */
  def all = from(table)(m => select(m))

  /**
   * same as find method.
   */
  def apply(id: K) = find(id)

  /**
   * search by id.
   */
  def find(id: K) = inTransaction { table.lookup(id) }

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

  /**
   * delete all records.
   */
  def deleteAll() = inTransaction {
    val models = all.toList
    models.foreach(_.delete)
    models
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

  def fromMap(data: Map[String, Any]) = {
    newInstance.assign(data)
  }
}

/**
 * Base class of ActiveRecord companion objects.
 *
 * This class provides database table mapping and query logic.
 */
trait ActiveRecordCompanion[T <: ActiveRecord] extends ActiveRecordBaseCompanion[Long, T] {
  implicit def toRichQueryA[A <: KeyedEntity[_]](r: ActiveRecordManyToMany[T, A])(implicit m: Manifest[T]) = RichQuery(r.relation)

  implicit def toModelList(r: ActiveRecordOneToMany[T]) = r.toList
  implicit def toModelListA[A <: KeyedEntity[_]](r: ActiveRecordManyToMany[T, A]) = r.toList
}

case class RichQuery[T <: ActiveRecordBase[_]](query: Queryable[T])(implicit m: Manifest[T]) {
  import org.squeryl.dsl.ast._

  val companion = ReflectionUtil.classToCompanion(m.erasure)
    .asInstanceOf[ActiveRecordBaseCompanion[_, T]]

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

  lazy val tables = this.getFields[Table[ActiveRecordBase[_]]].collect {
    case f if !classOf[IntermediateTable[_]].isAssignableFrom(f.getType) =>
      val name = getGenericTypes(f).last.getName
      (name, this.getValue[Table[ActiveRecordBase[_]]](f.getName))
  }.toMap

  /** All tables */
  lazy val all = tables.values

  override def tableNameFromClass(c: Class[_]) = super.tableNameFromClass(c).pluralize

  private def createTables = inTransaction {
    val isCreated = all.headOption.exists{ t =>
      val stat = Session.currentSession.connection.createStatement
      try {
        stat.execute("select 1 from " + t.name)
        true
      } catch {
        case e => false
      } finally {
        try { stat.close } catch {case e => }
      }
    }

    if (!isCreated) create
  }

  private var _initialized = false

  /** load configuration and then setup database and session */
  def initialize(implicit config: Map[String, Any] = Map()) {
    if (_initialized) return

    Config.conf = loadConfig(config)

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

  override protected def table[T]()(implicit m: Manifest[T]) =
    super.table[T]
    
  override protected def table[T](name: String)(implicit m: Manifest[T]) =
    if (m <:< manifest[IntermediateRecord]) new IntermediateTable[T](name)
    else super.table[T](name)

}

object Config {
  var conf: ActiveRecordConfig = _

  lazy val schema = ReflectionUtil.classToCompanion(conf.schemaClass)
    .asInstanceOf[ActiveRecordTables]

  def connection = conf.connection
  def adapter = conf.adapter

  def cleanup = conf.cleanup

  def translator = conf.translator
}

