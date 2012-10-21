package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import java.util.{Date, UUID}
import java.sql.Timestamp
import java.lang.annotation.Annotation
import mojolly.inflector.InflectorImports._

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

trait ActiveRecordBaseCompanion[K, T <: ActiveRecordBase[K]] extends ProductModelCompanion[T] with FormSupport[T] {
  import ReflectionUtil._

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
  implicit lazy val table: Table[T] = {
    val name = getClass.getName.dropRight(1)
    schema.tableMap(name).asInstanceOf[Table[T]]
  }

  /**
   * implicit conversion for query chain.
   */
  implicit def toRichQuery(query: Queryable[T])(implicit m: Manifest[T]): RichQuery[T] =
    RichQuery(query)

  implicit def toRichQuery(t: this.type)(implicit m: Manifest[T]): RichQuery[T] =
    RichQuery(t.table)

  implicit def toRichQuery(r: ActiveRecordOneToMany[T])
    (implicit m: Manifest[T]): RichQuery[T] = RichQuery(r.relation)

  implicit def toModel[A <: ActiveRecord](r: ActiveRecordManyToOne[A]): Option[A] = r.one

  implicit def toQueryable(t: this.type): Queryable[T] = t.table

  /**
   * all search.
   */
  def all: Query[T] = from(table)(m => select(m))

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
  def findBy(condition: (String, Any), conditions: (String, Any)*)
    (implicit query: Queryable[T]): Option[T] = findAllBy(condition, conditions:_*).headOption

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
    val info = fieldInfo.getOrElse(name,
      throw ActiveRecordException.notFoundField(name)
    )

    val clause = {m: T =>
      val v1 = m.getValue[Any](name) match {
        case o: Option[_] => o
        case v => Option(v)
      }
      val v2 = value match {
        case o: Option[_] => o
        case v => Option(v)
      }

      if (v2 == None && !info.isOption) {
        throw ActiveRecordException.unsupportedType(name + " by null")
      } else if (info.fieldType == classOf[String]) {
        v1.asInstanceOf[Option[String]] === v2.asInstanceOf[Option[String]]
      } else if (info.fieldType == classOf[Boolean]) {
        v1.asInstanceOf[Option[Boolean]] === v2.asInstanceOf[Option[Boolean]]
      } else if (info.fieldType == classOf[Int]) {
        v1.asInstanceOf[Option[Int]] === v2.asInstanceOf[Option[Int]]
      } else if (info.fieldType == classOf[Long]) {
        v1.asInstanceOf[Option[Long]] === v2.asInstanceOf[Option[Long]]
      } else if (info.fieldType == classOf[Float]) {
        v1.asInstanceOf[Option[Float]] === v2.asInstanceOf[Option[Float]]
      } else if (info.fieldType == classOf[Double]) {
        v1.asInstanceOf[Option[Double]] === v2.asInstanceOf[Option[Double]]
      } else if (info.fieldType == classOf[BigDecimal]) {
        v1.asInstanceOf[Option[BigDecimal]] === v2.asInstanceOf[Option[BigDecimal]]
      } else if (info.fieldType == classOf[Timestamp]) {
        v1.asInstanceOf[Option[Timestamp]] === v2.asInstanceOf[Option[Timestamp]]
      } else if (info.fieldType == classOf[Date]) {
        v1.asInstanceOf[Option[Date]] === v2.asInstanceOf[Option[Date]]
      } else if (info.fieldType == classOf[UUID]) {
        v1.asInstanceOf[Option[UUID]] === v2.asInstanceOf[Option[UUID]]
      } else {
        throw ActiveRecordException.unsupportedType(name + " by " + v2.toString)
      }
    }
    where(clause)(query)
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
  implicit def toRichQueryA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A])
    (implicit m: Manifest[T]): RichQuery[T] = RichQuery(r.relation)

  implicit def toModelList(r: ActiveRecordOneToMany[T]): List[T] = r.toList
  implicit def toModelListA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A]): List[T] = r.toList
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
  def orderBy(condition: (T) => OrderByExpression, conditions: (T => OrderByExpression)*): Query[T] =
    from(query)(m => select(m).orderBy((condition :: conditions.toList).map(_.apply(m))))

  /**
   * returns limited results.
   * {{{
   * Post.all.orderBy(p => p.updatedAt desc).limit(10)
   * }}}
   * @param count max count
   */
  def limit(count: Int): Query[T] = page(0, count)

  /**
   * returns page results.
   * {{{
   * Post.all.orderBy(p => p.updatedAt desc).page(10 * (pageNumber - 1), 10)
   * }}}
   * @param offset offset count
   * @param count max count
   */
  def page(offset: Int, count: Int): Query[T] = query match {
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

  def session: Session = Session.create(Config.connection, Config.adapter)

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
    table(m.erasure.getSimpleName.underscore.pluralize)(m)
  }

  def table[T <: ActiveRecordBase[_]](name: String)(implicit m: Manifest[T]): Table[T] = {
    if (m <:< manifest[IntermediateRecord]) {
      new IntermediateTable[T](name, this)
    } else {
      super.table[T](name)(m, dsl.keyedEntityDef(m))
    }
  }

}
