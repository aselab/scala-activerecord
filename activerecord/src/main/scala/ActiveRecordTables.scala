package com.github.aselab.activerecord

import org.squeryl.{Session, LazySession, AbstractSession, SessionFactory}
import org.squeryl.internals.DatabaseAdapter
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import com.github.aselab.activerecord.squeryl.Implicits._
import mojolly.inflector.InflectorImports._
import java.io.{PrintWriter, StringWriter}
import reflections.ReflectionUtil._
import java.sql.Connection
import scala.language.existentials

/**
 * Base class of database schema.
 */
trait ActiveRecordTables extends Schema {
  import reflections.ReflectionUtil._

  lazy val tableMap = {
    val c = classOf[ActiveRecord.HasAndBelongsToManyAssociation[_, _]]
    val map = collection.mutable.Map[String, Table[inner.IntermediateRecord]]()
    this.getFields[Table[ActiveRecordBase[_]]].map {f =>
      val clazz = getGenericTypes(f).last
      clazz.getDeclaredFields.foreach {f =>
        if (c.isAssignableFrom(f.getType)) {
          val List(c1, c2) = getGenericTypes(f)
          val tableName = tableNameFromClasses(c1, c2)
          if (!map.isDefinedAt(tableName)) {
            implicit val d = inner.IntermediateRecord.keyedEntityDef
            map(tableName) = super.table[inner.IntermediateRecord](tableName)
          }
        }
      }
      (clazz.getName, this.getValue[Table[AR]](f.getName))
    }.toMap ++ map
  }

  val sessionManager = new SessionManager(this)

  def inTransaction[T](f: => T): T = allSessions.find {
    case ActiveRecordSession(_, _, schema) if schema == this => true
    case _ => false
  } match {
    case Some(s) =>
      val old = Session.currentSession
      s.bindToCurrentThread
      val result = f
      old.bindToCurrentThread
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

  override def tableNameFromClass(c: Class[_]): String =
    c.getSimpleName.underscore.pluralize

  def tableNameFromClasses(c1: Class[_], c2: Class[_]): String =
    Seq(c1, c2).map(tableNameFromClass).sorted.mkString("_")

  def foreignKeyFromClass(c: Class[_]): String =
    c.getSimpleName.camelize + "Id"

  protected def execute(sql: String, logging: Boolean = true): Unit = inTransaction {
    if (logging) Config.logger.debug(sql)
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
        execute("select 1 from " + name + " limit 1", false)
        true
      } catch {
        case e: Throwable => false
      }
    }
  }

  private var _initialized = false

  /** load configuration and then setup database and session */
  def initialize(config: Map[String, Any]) {
    if (!_initialized) {
      configOption = Some(loadConfig(config))
      Config.registerSchema(this)
      if (Config.autoCreate) this.transaction { if (!isCreated) create }
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
    if (Config.autoDrop) drop
    Config.cleanup
    sessionStack.foreach { case (s1, s2) => s1.foreach(_.cleanup); s2.cleanup }
    sessionStack.clear
    _initialized = false
  }

  private var configOption: Option[ActiveRecordConfig] = None
  def config = configOption.getOrElse(throw ActiveRecordException.notInitialized)
  def loadConfig(c: Map[String, Any]): ActiveRecordConfig =
    new DefaultConfig(this, overrideSettings = c)

  def newSession: AbstractSession = {
    val connectionFunc = () => {
      val c = config.connection
      Config.logger.trace(Thread.currentThread.getStackTrace.toStream.drop(3).filterNot { se =>
        se.getClassName.startsWith("com.github.aselab") || se.getClassName.startsWith("org.squeryl")
      }.take(10).mkString("[StackTrace]\n  ", "\n  ", ""))
      Config.logger.debug("[URL] " + c.getMetaData.getURL)
      c
    }
    ActiveRecordSession(connectionFunc, config.adapter, this)
  }

  override def create = inTransaction { super.create }

  override def drop = inTransaction { super.drop }

  /** drop and create table */
  def reset: Unit = this.inTransaction {
    drop
    create
  }

  type SwapSession = (Option[AbstractSession], AbstractSession)
  private val sessionStack = collection.mutable.Stack.empty[SwapSession]

  def allSessions = Session.currentSessionOption.toSeq ++ sessionStack.map(_._2)

  /** Set rollback point for test */
  def startTransaction {
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
    val (oldSession, newSession) = sessionStack.pop
    newSession.unbindFromCurrentThread
    newSession.close
    oldSession.foreach(_.bindToCurrentThread)
  } catch {
    case e: NoSuchElementException => throw ActiveRecordException.cannotRollback
  }

  /** Rollback to startTransaction point */
  def rollback: Unit = try {
    val (oldSession, newSession) = sessionStack.pop
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

  def table[T <: AR]()(implicit m: Manifest[T]): Table[T] =
    table(tableNameFromClass(m.runtimeClass))(m)

  def table[T <: AR](name: String)(implicit m: Manifest[T]): Table[T] = {
    val t = super.table[T](name)(m, dsl.keyedEntityDef(m))

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
    }:_*))
    t
  }
}

class SessionManager(schema: ActiveRecordTables) extends SessionFactory {
  def newSession = schema.newSession
}

case class ActiveRecordSession(c: () => Connection, a: DatabaseAdapter, schema: ActiveRecordTables) extends LazySession(c, a) {
  setLogger(Config.logger.debug)
}

object ActiveRecordTables {
  def find(schemaName: String)(implicit classLoader: ClassLoader = defaultLoader): ActiveRecordTables =
    classToCompanion(schemaName).asInstanceOf[ActiveRecordTables]
}
