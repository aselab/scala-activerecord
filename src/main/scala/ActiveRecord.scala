package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.dsl.ast._
import org.squeryl.PrimitiveTypeMode._
import java.lang.annotation.Annotation
import mojolly.inflector.InflectorImports._
import squeryl.Implicits._

trait ProductModel extends Product with Saveable {
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
  def newInstance: T = classInfo.factory.apply

  /** ProductModel class information */
  lazy val classInfo: ClassInfo[T] = ClassInfo(targetClass)

  /** ProductModel fields information */
  lazy val fieldInfo: Map[String, FieldInfo] = classInfo.fieldInfo

  lazy val formatFields: List[java.lang.reflect.Field] = classInfo.fields

  lazy val validators: Map[String, Seq[(Annotation, Validator[_])]] = fieldInfo.map {
    case (name, info) => (name, info.annotations.flatMap { a =>
      Validator.get(a.annotationType).map(a -> _)
    })
  }.toMap
}

trait ActiveRecordBase[T] extends ProductModel with CRUDable
  with ActiveRecordBaseRelationSupport with ValidationSupport with IO
{
  def id: T
  def isPersisted: Boolean

  /** corresponding ActiveRecordCompanion object */
  lazy val recordCompanion = _companion.asInstanceOf[ActiveRecordBaseCompanion[T, this.type]]

  override def isNewRecord: Boolean = !isPersisted

  protected def doCreate = {
    recordCompanion.create(this)
    true
  }

  protected def doUpdate = {
    recordCompanion.update(this)
    true
  }

  protected def doDelete = recordCompanion.delete(id)

  protected lazy val relations: Map[(String, String), RelationWrapper[ActiveRecord, ActiveRecordBase[_]]] =
    recordCompanion.schema.relations
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

  def isPersisted: Boolean = id > 0
}

object ActiveRecord {
  case class Relation[T <: ActiveRecordBase[_]](
    conditions: Seq[T => LogicalBoolean] = Nil,
    orders: Seq[T => OrderByExpression] = Nil,
    pages: Option[(Int, Int)] = None
  )(implicit companion: ActiveRecordBaseCompanion[_, T], queryable: Queryable[T]) {

    private def whereState(m: T) =
      PrimitiveTypeMode.where(LogicalBoolean.and(conditions.map(_.apply(m))))

    private def ordersExpression(m: T) = orders.toList.map(_.apply(m))

    private def toQuery[R](selector: T => QueryYield[R]): Query[R] = {
      val query = from(queryable)(selector)
      pages.map {
        case (offset, count) => query.page(offset, count)
      }.getOrElse(query)
    }

    def where(condition: T => LogicalBoolean): Relation[T] =
      copy(conditions = conditions :+ condition)

    def findBy(condition: (String, Any), conditions: (String, Any)*): Option[T] =
      companion.findBy(condition, conditions:_*)(this)

    def findAllBy(condition: (String, Any), conditions: (String, Any)*): Relation[T] =
      companion.findAllBy(condition, conditions:_*)(this)

    def findBy(name: String, value: Any): Option[T] =
      companion.findBy(name, value)(this)

    def findAllBy(name: String, value: Any): Relation[T] =
      companion.findAllBy(name, value)(this)

    /**
     * sort results.
     *
     * {{{
     * Person.findAllBy("country", "Japan").orderBy(p => p.age asc)
     * Person.all.orderBy(p => p.age asc, p => p.name asc)
     * }}}
     * @param conditions sort conditions
     */
    def orderBy(conditions: (T => OrderByExpression)*): Relation[T] =
      copy(orders = orders ++ conditions.toList)

    /**
     * returns limited results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).limit(10)
     * }}}
     * @param count max count
     */
    def limit(count: Int): Relation[T] =
      copy(pages = pages.map(_._1 -> count).orElse(Some(0 -> count)))

    /**
     * returns page results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).page(10 * (pageNumber - 1), 10)
     * }}}
     * @param offset offset count
     * @param count max count
     */
    def page(offset: Int, count: Int): Relation[T] =
      copy(pages = Some((offset, count)))

    def count: Long = toQuery(m =>
      whereState(m).compute(PrimitiveTypeMode.count)
    )

    def toQuery: Query[T] = toQuery[T](m =>
      whereState(m).select(m).orderBy(ordersExpression(m))
    )
  }
}

trait ActiveRecordBaseCompanion[K, T <: ActiveRecordBase[K]] extends ProductModelCompanion[T] with FormSupport[T] {
  import ReflectionUtil._
  import ActiveRecord._

  implicit val keyedEntityDef = new KeyedEntityDef[T, K] {
    def getId(m: T) = m.id
    def isPersisted(m: T) = m.isPersisted
    def idPropertyName = "id"
  }

  implicit def relationToIterable[A <% Relation[T]](relation: A): Iterable[T] =
    inTransaction { relation.toQuery.toList }

  /** self reference */
  protected def self: this.type = this

  /** database schema */
  lazy val schema = Config.schema

  /**
   * corresponding database table
   */
  implicit lazy val table: Table[T] = {
    val name = getClass.getName.dropRight(1)
    schema.tableMap(name).asInstanceOf[Table[T]]
  }

  /**
   * implicit conversion for query chain.
   */
  implicit def toRelation(query: Queryable[T]): Relation[T] =
    Relation[T]()(this, query)

  implicit def toRelation(r: ActiveRecordOneToMany[T]): Relation[T] =
    toRelation(r.relation)

  implicit def toRelation(t: this.type): Relation[T] = all

  implicit def toModel[A <: ActiveRecord](r: ActiveRecordManyToOne[A]): Option[A] = r.one

  implicit def toQueryable(t: this.type): Queryable[T] = t.table

  /**
   * all search.
   */
  implicit def all: Relation[T] = toRelation(table)

  /**
   * same as find method.
   */
  def apply(id: K): Option[T] = find(id)

  /**
   * search by id.
   */
  def find(id: K): Option[T] = inTransaction { table.lookup(id) }

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
  def where(condition: (T) => LogicalBoolean)(implicit relation: Relation[T]) =
    relation.where(condition)

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
  def findBy(condition: (String, Any), conditions: (String, Any)*)
    (implicit relation: Relation[T]): Option[T] = inTransaction {
    findAllBy(condition, conditions:_*)(relation).limit(1).headOption
  }

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
  def findAllBy(condition: (String, Any), conditions: (String, Any)*)
    (implicit relation: Relation[T]): Relation[T] = {
    conditions.foldLeft(findAllBy(condition._1, condition._2)(relation)) {
      case (r, cond) => findAllBy(cond._1, cond._2)(r)
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
  def findBy(name: String, value: Any)
    (implicit relation: Relation[T]): Option[T] = inTransaction {
    findAllBy(name, value)(relation).limit(1).headOption
  }

  /**
   * Search by fieldname and value.
   * {{{
   * findAllBy("name", "abc")
   * }}}
   * @param name field name
   * @param value field value
   * @param query table or subquery in from clause. default is table
   */
  def findAllBy(name: String, value: Any)(implicit relation: Relation[T]): Relation[T] = {
    val field = fieldInfo.getOrElse(name,
      throw ActiveRecordException.notFoundField(name)
    )

    val clause = {m: T =>
      val v1 = m.getValue[Any](name)
      val v2 = value
      field.toEqualityExpression(v1, v2)
    }
    where(clause)(relation)
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
  def deleteAll(): List[T] = inTransaction {
    val models = all.toList
    models.foreach(_.delete)
    models
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
          findBy(name, value).isEmpty
        case Some(_) => true
        case None => findBy(name, value).isEmpty
      }
    }
  }

  /** Unique annotated fields */
  lazy val uniqueFields =
    formatFields.filter(_.isAnnotationPresent(classOf[annotations.Unique]))

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
  import ActiveRecord._

  implicit def toRelationA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A]): Relation[T] = toRelation(r.relation)

  implicit def toModelList(r: ActiveRecordOneToMany[T]): List[T] = r.toList
  implicit def toModelListA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A]): List[T] = r.toList
}

/**
 * Base class of database schema.
 */
trait ActiveRecordTables extends Schema with TableRelationSupport {
  import ReflectionUtil._

  lazy val tableMap = this.getFields[Table[ActiveRecordBase[_]]].collect {
    case f if !classOf[IntermediateTable[_]].isAssignableFrom(f.getType) =>
      val name = getGenericTypes(f).last.getName
      (name, this.getValue[Table[ActiveRecordBase[_]]](f.getName))
  }.toMap

  /** All tables */
  lazy val all = tableMap.values

  override def columnNameFromPropertyName(propertyName: String): String  =
    propertyName.underscore
    
  override def tableNameFromClass(c: Class[_]): String =
    c.getSimpleName.underscore.pluralize

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
    if (!_initialized) {
      Config.conf = loadConfig(config)

      SessionFactory.concreteFactory = Some(() => session)

      createTables
    }

    _initialized = true
  }

  /** cleanup database resources */
  def cleanup: Unit = Config.cleanup

  def loadConfig(config: Map[String, Any]): ActiveRecordConfig =
    new DefaultConfig(overrideSettings = config)

  def session: Session = {
    val s = Session.create(Config.connection, Config.adapter)
    s.setLogger(Config.logger.debug)
    s
  }

  /** drop and create table */
  def reset: Unit = inTransaction {
    drop
    create
  }

  private var _session: (Option[Session], Option[Session]) = (None, None)

  /** Set rollback point for test */
  def start {
    val oldSession = Session.currentSessionOption
    val newSession = SessionFactory.newSession
    oldSession.foreach(_.unbindFromCurrentThread)
    newSession.bindToCurrentThread
    val c = newSession.connection
    try {
      if (c.getAutoCommit) c.setAutoCommit(false)
    } catch { case e => }
    _session = (oldSession, Option(newSession))
  }

  /** Rollback to start point */
  def clean: Unit = _session match {
    case (oldSession, Some(newSession)) =>
      newSession.connection.rollback
      newSession.unbindFromCurrentThread
      oldSession.foreach(_.bindToCurrentThread)
      _session = (None, None)
    case _ =>
      throw ActiveRecordException.cannotCleanSession
  }

  def table[T <: ActiveRecordBase[_]]()(implicit m: Manifest[T]): Table[T] = {
    table(tableNameFromClass(m.erasure))(m)
  }

  def table[T <: ActiveRecordBase[_]](name: String)(implicit m: Manifest[T]): Table[T] = {
    val t = if (m <:< manifest[IntermediateRecord]) {
      new IntermediateTable[T](name, this)
    } else {
      super.table[T](name)(m, dsl.keyedEntityDef(m))
    }

    val c = classToCompanion(m.erasure).asInstanceOf[ActiveRecordBaseCompanion[_, T]]
    val fields = c.fieldInfo.values.toSeq
    import annotations._

    // schema declarations
    on(t)(r => declare(fields.collect {
      case f if f.hasAnnotation[Unique] =>
        f.toExpression(r.getValue[Any](f.name)).is(unique)
        
      case f if f.hasAnnotation[Confirmation] =>
        val name = Validator.confirmationFieldName(f.name, f.getAnnotation[Confirmation])
        f.toExpression(r.getValue[Any](name)).is(transient)
    }:_*))
    t
  }

}
