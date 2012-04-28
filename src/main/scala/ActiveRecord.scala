package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.internals.DatabaseAdapter
import org.squeryl.adapters._
import org.squeryl.PrimitiveTypeMode._
import java.util.{Date, UUID}
import java.sql.{Timestamp, DriverManager, Connection}
import com.jolbox.bonecp._
import com.typesafe.config._
import mojolly.inflector.InflectorImports._

/**
 * Base class of ActiveRecord objects.
 *
 * This class provides object-relational mapping and CRUD logic and callback hooks.
 */
abstract class ActiveRecord extends KeyedEntity[Long] with Product with CRUDable {
  import ReflectionUtil._

  /** primary key */
  val id: Long = 0L

  private[activerecord] def setId(id: Long) = {
    val f = classOf[ActiveRecord].getDeclaredField("id")
    f.setAccessible(true)
    f.set(this, id)
  }

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

  def map(newValues: (String, Any)*) = {
    val constructor = this.getClass.getConstructors.head
    val n = constructor.newInstance(productIterator.map(_.asInstanceOf[AnyRef]).toSeq:_*).asInstanceOf[this.type]
    (_companion.formatFields.map {
      f => (f.getName, this.getValue[Any](f.getName))
    }.toMap ++ newValues).foreach {
      case (k, v) => n.setValue(k, v)
    }
    n.setId(id)
    n
  }

  def apply(newValues: (String, Any)*) = map(newValues:_*)

  private def getRelation(left: Class[_], right: Class[_]) =
    _companion.schema.relations.getOrElse(
      left.getName -> right.getName,
      ActiveRecordException.missingRelation
    )

  protected def belongsTo[T <: ActiveRecord](implicit m: Manifest[T]) =
    getRelation(m.erasure, getClass).belongsTo(this).asInstanceOf[ActiveRecordManyToOne[T]]

  protected def hasMany[T <: ActiveRecord](implicit m: Manifest[T]) =
    getRelation(getClass, m.erasure).hasMany(this).asInstanceOf[ActiveRecordOneToMany[T]]

  def toMap(implicit excludeRelation: Boolean = false): Map[String, Any] = {
    def relationMap(o: Any) = o.asInstanceOf[ActiveRecord].toMap(true)
    _companion.formatFields.flatMap { f =>
      val name = f.getName
      (this.getValue[Any](name) match {
        case r: RecordRelation if excludeRelation => None
        case ActiveRecordOneToMany(r) => Some(r.toList.map(relationMap))
        case ActiveRecordManyToOne(r) => r.headOption.map(relationMap)
        case v: Option[_] => v
        case v => Some(v)
      }).map(name -> _)
    }.toMap
  }
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
  implicit def toRichQuery(query: Query[T])(implicit m: Manifest[T]) =
    RichQuery(query)

  implicit def toRichQuery(r: ActiveRecordOneToMany[T])
    (implicit m: Manifest[T]) = RichQuery(r.relation)

  implicit def toModelList(r: ActiveRecordOneToMany[T]) = r.toList
  implicit def toModel(r: ActiveRecordManyToOne[T]) = r.one

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
   * findBy {m: T => m.name === "abc" and m.age > 20}
   * }}}
   *
   * @param condition search condition
   * @param query table or subquery in from clause. default is table
   */
  def where(condition: (T) => org.squeryl.dsl.ast.LogicalBoolean)(implicit query: Queryable[T]): Query[T] = {
    from(query)(m => PrimitiveTypeMode.where(condition(m)) select(m))
  }

  /**
   * Search by multiple fieldnames and values.
   *
   * {{{
   * 例: findBy("name" -> "abc", "email" -> "abc@foo.bar")
   * }}}
   * @param condition fieldname-value tuple
   * @param conditions multiple fieldname-value tuples(optional)
   * @param query table or subquery in from clause. default is table
   */
  def findBy(condition: (String, Any), conditions: (String, Any)*)(implicit query: Queryable[T]): Query[T] = {
    conditions.foldLeft(findBy(condition._1, condition._2)(query)) {
      case (subquery, cond) => findBy(cond._1, cond._2)(subquery)
    }
  }

  /**
   * Search by fieldname and value.
   * {{{
   * 例: findBy("name", "abc")
   * }}}
   * @param name field name
   * @param value field value
   * @param query table or subquery in from clause. default is table
   */
  def findBy(name: String, value: Any)(implicit query: Queryable[T]): Query[T] = {
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
  def deleteAll = inTransaction {
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

case class RichQuery[T <: ActiveRecord](query: Query[T])(implicit m: Manifest[T]) {
  val companion = ReflectionUtil.classToCompanion(m.erasure)
    .asInstanceOf[ActiveRecordCompanion[T]]

  def where(condition: (T) => org.squeryl.dsl.ast.LogicalBoolean): Query[T] = {
    companion.where(condition)(query)
  }

  def findBy(condition: (String, Any), conditions: (String, Any)*): Query[T] = {
    companion.findBy(condition, conditions:_*)(query)
  }

  def findBy(name: String, value: Any): Query[T] = {
    companion.findBy(name, value)(query)
  }

  /**
   * sort.
   *
   * {{{
   * Person.findBy("country", "Japan").orderBy(p => p.age asc)
   * Person.all.orderBy(p => p.age asc, p => p.name asc)
   * }}}
   * @param condition sort condition
   * @param conditions multiple sort conditions(optional)
   */
  def orderBy(condition: (T) => org.squeryl.dsl.ast.OrderByArg, conditions: (T => org.squeryl.dsl.ast.OrderByArg)*) = {
    conditions.toList match {
      case Nil => from(query)(m => select(m).orderBy(condition(m)))
      case List(f1) => from(query)(m => select(m).orderBy(condition(m), f1(m)))
      case List(f1, f2) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m)))
      case List(f1, f2, f3) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m), f3(m)))
      case List(f1, f2, f3, f4) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m), f3(m), f4(m)))
      case List(f1, f2, f3, f4, f5) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m), f3(m), f4(m), f5(m)))
      case List(f1, f2, f3, f4, f5, f6) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m), f3(m), f4(m), f5(m), f6(m)))
    }
  }

  /**
   * limit query.
   * {{{
   * Post.all.orderBy(p => p.updatedAt desc).limit(10)
   * }}}
   * @param count max count
   */
  def limit(count: Int) = query.page(0, count)
}

/**
 * Base class of database schema.
 */
trait ActiveRecordTables extends Schema {
  import ReflectionUtil._
  import com.github.aselab.activerecord.{ActiveRecord => AR}

  lazy val tables = this.getFields[Table[AR]].map {f =>
    val name = getGenericType(f).getName
    (name, this.getValue[Table[AR]](f.getName))
  }.toMap

  lazy val relations = {
    this.getFields[Relation[AR, AR]].map {f =>
      val types = getGenericTypes(f).map(_.getName)
      val left = types.head
      val right = types.last
      val relation = this.getValue[Relation[AR, AR]](f.getName)

      (left, right) -> RelationWrapper(relation)
    }.toMap
  }

  /** All tables */
  lazy val all = tables.values

  override def tableNameFromClass(c: Class[_]) = super.tableNameFromClass(c).pluralize

  def foreignKeyName(c: Class[_]) = c.getSimpleName.underscore.camelize + "Id"

  def oneToMany[O <: AR, M <: AR](ot: Table[O], mt:Table[M])(implicit om: Manifest[O], mm: Manifest[M]) = {
    val foreignKey = foreignKeyName(om.erasure)
    val foreignKeyIsOption= try {
      val f = mm.erasure.getDeclaredField(foreignKey)
      f.getType.getName == "scala.Option"
    } catch {
      case e: java.lang.NoSuchFieldException =>
        ActiveRecordException.missingForeignKey(foreignKey)
    }

    oneToManyRelation(ot, mt).via {(o, m) => 
      if (foreignKeyIsOption) {
        o.id === m.getValue[Option[Long]](foreignKey)
      } else {
        o.id === m.getValue[Long](foreignKey)
      }
    }
  }

  private lazy val createTables = transaction {
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
    DefaultConfig(ConfigFactory.load(), config)

  def session = Session.create(Config.connection, Config.adapter)

  /** drop and create table */
  def reset = transaction {
    drop
    create
  }
}

trait ActiveRecordConfig {
  def schemaClass: String
  def connection: Connection
  def adapter: DatabaseAdapter
  def cleanup: Unit = {
    Session.cleanupResources
  }
}

case class DefaultConfig(conf: Config, map: Map[String, Any]) extends ActiveRecordConfig {
  val env = System.getProperty("run.mode", "dev")

  def get[T](key: String): Option[T] = map.get(key).map(_.asInstanceOf[T])
  def get[T](key: String, getter: String => T): Option[T] = try {
    Option(getter(env + "." + key))
  } catch {
    case e: ConfigException.Missing => None
  }
  def getString(key: String) = get[String](key).orElse(get(key, conf.getString))
  def getInt(key: String) = get[Int](key).orElse(get(key, conf.getInt))

  lazy val schemaClass = getString("schema").getOrElse("models.Tables")
  lazy val driverClass = getString("driver").getOrElse("org.h2.Driver")
  lazy val jdbcurl = getString("jdbcurl").getOrElse("jdbc:h2:mem:activerecord")
  lazy val username = getString("username")
  lazy val password = getString("password")
  lazy val partitionCount = getInt("partitionCount")
  lazy val maxConnectionsPerPartition = getInt("maxConnectionsPerPartition")
  lazy val minConnectionsPerPartition = getInt("minConnectionsPerPartition")

  lazy val adapter = driverClass match {
    case "org.h2.Driver" => new H2Adapter
    case "org.postgresql.Driver" => new PostgreSqlAdapter
    case "com.mysql.jdbc.Driver" => new MySQLAdapter
    case driver => ActiveRecordException.unsupportedDriver(driver)
  }

  lazy val pool = {
    try {
      Class.forName(driverClass)
    } catch {
      case e => ActiveRecordException.missingDriver(driverClass)
    }

    val conf = new BoneCPConfig
    conf.setJdbcUrl(jdbcurl)
    username.foreach(conf.setUsername(_))
    password.foreach(conf.setPassword(_))
    partitionCount.foreach(conf.setPartitionCount(_))
    maxConnectionsPerPartition.foreach(conf.setMaxConnectionsPerPartition(_))
    minConnectionsPerPartition.foreach(conf.setMinConnectionsPerPartition(_))
    new BoneCP(conf)
  }

  override def cleanup = {
    super.cleanup
    pool.shutdown
  }

  def connection = pool.getConnection
}

object Config {
  var conf: ActiveRecordConfig = _

  lazy val schema = ReflectionUtil.classToCompanion(conf.schemaClass)
    .asInstanceOf[ActiveRecordTables]

  def connection = conf.connection
  def adapter = conf.adapter

  def cleanup = conf.cleanup
}

class ActiveRecordException(msg: String) extends RuntimeException(msg)

object ActiveRecordException {
  def unsupportedType(name: String) =
    throw new ActiveRecordException("Unsupported type: " + name)

  def defaultConstructorRequired =
    throw new ActiveRecordException("Must implement default constructor")

  def optionValueMustBeSome =
    throw new ActiveRecordException("Cannot detect generic type parameter when a field's default value is None because of type erasure.")

  def traversableValueMustNotBeNil =
    throw new ActiveRecordException("Cannot detect generic type parameter when a field's default value is Nil because of type erasure.")

  def cannotDetectType(value: Any) =
    throw new ActiveRecordException("Cannot detect type of %s.".format(value))

  def unsupportedDriver(driver: String) =
    throw new ActiveRecordException("Unsupported database driver: " + driver)

  def missingDriver(driver: String) =
    throw new ActiveRecordException("Cannot load database driver: " + driver)

  def missingRelation =
    throw new ActiveRecordException("Cannot find definition of relation")

  def missingForeignKey(name: String) =
    throw new ActiveRecordException("Cannot find declaration of foreign key: " + name)
}

