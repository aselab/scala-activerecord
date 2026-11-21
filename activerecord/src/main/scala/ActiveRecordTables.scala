package com.github.aselab.activerecord

import org.squeryl.{Session, LazySession, AbstractSession, SessionFactory}
import org.squeryl.internals.DatabaseAdapter
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import com.github.aselab.activerecord.squeryl.Implicits._
import util.InflectorImports._
import java.io.{PrintWriter, StringWriter}
import reflections.ReflectionUtil
import java.sql.Connection
import scala.language.existentials
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/**
 * Base class of database schema.
 */
trait ActiveRecordTables extends Schema {
  import ReflectionUtil.{classToARCompanion, getGenericTypes, toReflectable}

  lazy val tableMap = {
    val c = classOf[ActiveRecord.HasAndBelongsToManyAssociation[?, ?]]
    val map = collection.mutable.Map[String, Table[inner.IntermediateRecord]]()
    this.getFields[Table[ActiveRecordBase[?]]].map {f =>
      val t = this.getValue[Table[AR]](f.getName)
      // take the model class from the table: Scala 3.7+ emits no generic signature for a val inherited from a trait
      val clazz = t.posoMetaData.clasz
      // associations are usually lazy vals, whose backing field is untyped on Scala 3: use the accessors
      clazz.getDeclaredMethods.foreach {m =>
        if (m.getParameterCount == 0 && c.isAssignableFrom(m.getReturnType)) {
          val List(c1, c2) = getGenericTypes(m.getGenericReturnType): @unchecked
          val tableName = tableNameFromClasses(c1, c2)
          if (!map.isDefinedAt(tableName)) {
            implicit val d = inner.IntermediateRecord.keyedEntityDef
            map(tableName) = super.table[inner.IntermediateRecord](tableName)
          }
        }
      }
      (clazz.getName, t)
    }.toMap ++ map
  }

  val sessionManager = new SessionManager(this)

  def inTransaction[T](f: => T): T = allSessions.find {
    case ActiveRecordSession(_, _, schema) if schema == this => true
    case _ => false
  } match {
    case Some(s) =>
      val old = Session.currentSessionOption
      s.bindToCurrentThread
      val result = f
      old.foreach(_.bindToCurrentThread)
      result
    case None =>
      transaction(f)
  }

  def transaction[T](f: => T): T = {
    dsl.transaction(this.sessionManager)(f)
  }

  def getTable[T](name: String): Table[T] = {
    tableMap.getOrElse(name, throw ActiveRecordException.tableNotFound(name))
      .asInstanceOf[Table[T]]
  }

  /** All tables */
  lazy val all = tableMap.values

  override def columnNameFromPropertyName(propertyName: String): String =
    propertyName.underscore

  override def tableNameFromClass(c: Class[?]): String =
    tableNameFromClass(c.getSimpleName)

  def tableNameFromClass(className: String): String =
    className.underscore.pluralize

  def tableNameFromClasses(c1: Class[?], c2: Class[?]): String =
    Seq(c1, c2).map(tableNameFromClass).sorted.mkString("_")

  def foreignKeyFromClass(c: Class[?]): String =
    c.getSimpleName.camelize + "Id"

  protected def execute(sql: String, logging: Boolean = true): Boolean = inTransaction {
    if (logging) config.logger.debug(sql)
    val connection = Session.currentSession.connection
    val s = connection.createStatement
    try {
      s.execute(sql)
    } catch {
      case e: java.sql.SQLException =>
        connection.rollback
        throw ActiveRecordException("error executing " + sql + "\n" + e)
    } finally {
      s.close
    }
  }

  private[activerecord] def isCreated: Boolean = inTransaction {
    all.headOption.exists{ t =>
      try {
        val name = config.adapter.quoteName(t.prefixedName)
        execute("select 1 from " + name + " where 0 = 1", false)
        true
      } catch {
        case NonFatal(_) => false
      }
    }
  }

  private var _initialized = false

  /** load configuration and then setup database and session */
  def initialize(config: Map[String, Any]): Unit = {
    if (!_initialized) {
      configOption = Some(loadConfig(config))
      Config.registerSchema(this)
      if (this.config.autoCreate) this.transaction { if (!isCreated) create }
    }

    _initialized = true
  }

  def initialize: Unit = initialize(Map())

  def apply[T](config: Map[String, Any] = Map())(f: => T): T = {
    initialize(config)
    try { f } finally { cleanup }
  }

  /** cleanup database resources */
  def cleanup: Unit = {
    if (config.autoDrop) drop
    config.cleanup
    sessionStack.foreach { case (s1, s2) => s1.foreach(_.cleanup); s2.cleanup }
    sessionStack.clear()
    _initialized = false
  }

  private var configOption: Option[ActiveRecordConfig] = None
  def config: ActiveRecordConfig = configOption.getOrElse(throw ActiveRecordException.notInitialized)
  def loadConfig(c: Map[String, Any]): ActiveRecordConfig =
    new DefaultConfig(this, overrideSettings = c)

  def newSession: AbstractSession = {
    val connectionFunc = () => {
      val c = config.connection
      config.logger.trace(Thread.currentThread.getStackTrace.to(LazyList).drop(3).filterNot { se =>
        se.getClassName.startsWith("com.github.aselab") || se.getClassName.startsWith("org.squeryl")
      }.take(10).mkString("[StackTrace]\n  ", "\n  ", ""))
      config.logger.debug("[URL] " + c.getMetaData.getURL)
      c
    }
    ActiveRecordSession(connectionFunc, config.adapter, this)
  }

  override def create: Unit = inTransaction { super.create }

  override def drop: Unit = inTransaction { super.drop }

  /** drop and create table */
  def reset: Unit = this.inTransaction {
    drop
    create
  }

  type SwapSession = (Option[AbstractSession], AbstractSession)
  private val sessionStack = collection.mutable.Stack.empty[SwapSession]

  def allSessions: Seq[AbstractSession] = sessionStack.map(_._2).toSeq ++ Session.currentSessionOption.toSeq

  /** Set rollback point for test */
  def startTransaction: Unit = {
    val oldSession = Session.currentSessionOption
    val newSession = sessionManager.newSession
    oldSession.foreach(_.unbindFromCurrentThread)
    newSession.bindToCurrentThread
    val c = newSession.connection
    try {
      if (c.getAutoCommit) c.setAutoCommit(false)
    } catch { case e: java.sql.SQLException => }
    sessionStack.push(oldSession -> newSession)
  }

  def endTransaction: Unit = try {
    val (oldSession, newSession) = sessionStack.pop()
    newSession.unbindFromCurrentThread
    newSession.close
    oldSession.foreach(_.bindToCurrentThread)
  } catch {
    case e: NoSuchElementException => throw ActiveRecordException.cannotRollback
  }

  /** Rollback to startTransaction point */
  def rollback: Unit = try {
    val (oldSession, newSession) = sessionStack.pop()
    newSession.connection.rollback
    newSession.unbindFromCurrentThread
    newSession.close
    oldSession.foreach(_.bindToCurrentThread)
  } catch {
    case e: NoSuchElementException => throw ActiveRecordException.cannotRollback
  }

  def withRollback[T](f: => T): T = {
    startTransaction
    try { f } finally { rollback }
  }

  def ddl: String = inTransaction {
    val out = new StringWriter
    printDdl(new PrintWriter(out))
    out.toString
  }

  /**
   * Defines the table of model `T`. Inline so that Squeryl receives the element types of
   * `Option[Int]`-like fields, which it can only obtain at compile time on Scala 3.
   */
  inline def table[T <: AR](implicit m: ClassTag[T]): Table[T] =
    tableWithFieldsInfo[T](defaultTableName(m.runtimeClass), org.squeryl.internals.TypeInfo.fieldsInfo[T])

  inline def table[T <: AR](name: String)(implicit m: ClassTag[T]): Table[T] =
    tableWithFieldsInfo[T](name, org.squeryl.internals.TypeInfo.fieldsInfo[T])

  /** Defines the table of model `T` in database schema `prefix`. */
  inline def table[T <: AR](name: String, prefix: String)(implicit m: ClassTag[T]): Table[T] =
    prefixedTableWithFieldsInfo[T](name, prefix, org.squeryl.internals.TypeInfo.fieldsInfo[T])

  def prefixedTableWithFieldsInfo[T <: AR](name: String, prefix: String, fieldsInfo: Map[String, Class[?]])(
    implicit m: ClassTag[T]
  ): Table[T] = super.table[T](name, prefix)(fieldsInfo)(using m, dsl.keyedEntityDef(using m))

  /** table name of `typeT`: the root model's name for STI models */
  def defaultTableName(typeT: Class[?]): String =
    baseARType(typeT).map(c => tableNameFromClass(c)).getOrElse(tableNameFromClass(typeT))

  def tableWithFieldsInfo[T <: AR](name: String, fieldsInfo: Map[String, Class[?]])(implicit m: ClassTag[T]): Table[T] = {
    val t = super.table[T](name)(fieldsInfo)(using m, dsl.keyedEntityDef(using m))

    val c = classToARCompanion[T](m.runtimeClass)
    val fields = c.fieldInfo.values.toSeq
    import annotations._

    // schema declarations
    on(t)(r => declare(fields.collect {
      case f if f.hasAnnotation[Unique] =>
        f.toExpression(r.getValue[Any](f.name)).is(unique)

      case f if f.hasAnnotation[Confirmation] =>
        val name = validations.Validator.confirmationFieldName(f.name, f.getAnnotation[Confirmation])
        f.toExpression(r.getValue[Any](name)).is(transient)
    }*))
    t
  }
}

class SessionManager(schema: ActiveRecordTables) extends SessionFactory {
  def newSession: AbstractSession = schema.newSession
}

case class ActiveRecordSession(
  c: () => Connection, a: DatabaseAdapter, schema: ActiveRecordTables
) extends LazySession(c, a) {
  setLogger(Config.logger.debug)
}

object ActiveRecordTables {
  import ReflectionUtil.{defaultLoader, classToCompanion}

  def find(schemaName: String)(implicit classLoader: ClassLoader = defaultLoader): ActiveRecordTables =
    classToCompanion(schemaName).asInstanceOf[ActiveRecordTables]
}
