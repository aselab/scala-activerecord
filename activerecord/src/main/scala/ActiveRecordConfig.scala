package com.github.aselab.activerecord

import org.squeryl.Session
import org.squeryl.internals.DatabaseAdapter
import org.squeryl.adapters._
import java.sql.Connection
import java.util.TimeZone
import com.jolbox.bonecp._
import com.typesafe.config._
import org.slf4j.{Logger, LoggerFactory}
import scala.util.control.Exception.catching
import reflections.ReflectionUtil._
import scala.collection.JavaConversions._

object Config {
  private var _conf: ActiveRecordConfig = _
  private var _timeZone: TimeZone = _

  def confOption: Option[ActiveRecordConfig] = Option(_conf)
  def conf: ActiveRecordConfig = confOption.getOrElse(throw ActiveRecordException.notInitialized)
  def conf_=(value: ActiveRecordConfig): Unit = _conf = value

  def schema(companion: ActiveRecordBaseCompanion[_, _]): ActiveRecordTables = {
    val clazz = companion.classInfo.clazz
    tables.getOrElse(clazz, throw ActiveRecordException.tableNotFound(clazz.toString))
  }

  def cleanup: Unit = conf.cleanup

  def translator: i18n.Translator =
    confOption.map(_.translator).getOrElse(i18n.DefaultTranslator)

  def timeZone: TimeZone = Option(_timeZone).getOrElse(conf.timeZone)
  def timeZone_=(value: TimeZone): Unit = _timeZone = value
  def logger: Logger = conf.logger
  def classLoader: Option[ClassLoader] = confOption.map(_.classLoader)

  private val tables = collection.mutable.Map.empty[Class[_], ActiveRecordTables]
  def registerSchema(s: ActiveRecordTables) = {
    conf = s.config
    s.all.foreach(t => tables.update(t.posoMetaData.clasz, s))
  }

  def loadSchemas(key: String = "schemas", config: Config = ConfigFactory.load, classLoader: ClassLoader = defaultLoader) =
    config.getStringList(key).map(ActiveRecordTables.find)
}

trait ActiveRecordConfig {
  def connection: Connection
  def adapter: DatabaseAdapter

  def adapter(driverClass: String): DatabaseAdapter = driverClass match {
    case "org.h2.Driver" => new H2Adapter
    case "org.postgresql.Driver" => new PostgreSqlAdapter
    case "com.mysql.jdbc.Driver" => new MySQLAdapter {
      override def quoteIdentifier(s: String) = "`%s`".format(s)
    }
    case "oracle.jdbc.OracleDriver" => new OracleAdapter
    case "org.apache.derby.jdbc.EmbeddedDriver" => new DerbyAdapter
    case driver => throw ActiveRecordException.unsupportedDriver(driver)
  }

  def cleanup: Unit = {
    Session.cleanupResources
  }
  def translator: i18n.Translator
  def timeZone: TimeZone
  def classLoader: ClassLoader

  lazy val logger = LoggerFactory.getLogger("activerecord")
}

class DefaultConfig(
  schema: ActiveRecordTables,
  config: Config = ConfigFactory.load(),
  overrideSettings: Map[String, Any] = Map()
) extends ActiveRecordConfig {
  val env = System.getProperty("run.mode", "dev")
  val _prefix = schema.getClass.getName.dropRight(1)
  def prefix(key: String) = _prefix + "." + key

  logger.debug("----- Loading config: %s (mode: %s) -----".format(_prefix, env))

  private def debug[T](key: String, value: Option[T], default: String = "(not found)") =
    logger.debug("\t%s -> %s".format(key, value.getOrElse(default)))

  def get[T](key: String): Option[T] = {
    if (overrideSettings.isEmpty) return None
    val keyWithPrefix = prefix(key)
    val valueWithPrefix = overrideSettings.get(keyWithPrefix)
    debug(keyWithPrefix + " (overrideSettings)", valueWithPrefix)
    if (valueWithPrefix.isEmpty) {
      debug(key + " (overrideSettings)", overrideSettings.get(key))
    }
    valueWithPrefix.orElse(overrideSettings.get(key)).map(_.asInstanceOf[T])
  }

  def get[T](key: String, getter: String => T): Option[T] = {
    def inner(k: String) = try {
      Option(getter(k))
    } catch {
      case e: ConfigException.Missing => None
    }
    val k = env + "." + key
    val keyWithPrefix = prefix(k)
    val valueWithPrefix = inner(keyWithPrefix)
    debug(keyWithPrefix, valueWithPrefix)
    if (valueWithPrefix.isEmpty) {
      debug(k, inner(k))
    }
    valueWithPrefix.orElse(inner(k))
  }
  def getString(key: String): Option[String] = get[String](key).orElse(get(key, config.getString))
  def getInt(key: String): Option[Int] = get[Int](key).orElse(get(key, config.getInt))

  lazy val driverClass = getString("driver").getOrElse("org.h2.Driver")
  lazy val jdbcurl = getString("jdbcurl").getOrElse("jdbc:h2:mem:activerecord")
  lazy val username = getString("username")
  lazy val password = getString("password")
  lazy val partitionCount = getInt("partitionCount")
  lazy val maxConnectionsPerPartition = getInt("maxConnectionsPerPartition")
  lazy val minConnectionsPerPartition = getInt("minConnectionsPerPartition")

  lazy val adapter: DatabaseAdapter = adapter(driverClass)
  def classLoader: ClassLoader = Thread.currentThread.getContextClassLoader

  lazy val pool = {
    try {
      Class.forName(driverClass)
    } catch {
      case e: ClassNotFoundException => throw ActiveRecordException.missingDriver(driverClass)
    }

    val conf = new BoneCPConfig
    conf.setJdbcUrl(jdbcurl)
    username.foreach(conf.setUsername)
    password.foreach(conf.setPassword)
    partitionCount.foreach(conf.setPartitionCount)
    maxConnectionsPerPartition.foreach(conf.setMaxConnectionsPerPartition)
    minConnectionsPerPartition.foreach(conf.setMinConnectionsPerPartition)
    logger.debug("----- Database setting: %s (mode: %s) -----".format(_prefix, env))
    settings.foreach{ case (k, v) => debug(k, v, "") }
    new BoneCP(conf)
  }

  lazy val settings = List(
    "driver" -> Some(driverClass),
    "jdbcurl" -> Some(jdbcurl),
    "username" -> username,
    "password" -> password,
    "maxConnectionsPerPartition" -> maxConnectionsPerPartition,
    "minConnectionsPerPartition" -> minConnectionsPerPartition
  )

  override def cleanup: Unit = {
    super.cleanup
  }

  def connection: Connection = pool.getConnection
  val translator: i18n.Translator = i18n.DefaultTranslator
  lazy val timeZone = getString("timeZone").map(TimeZone.getTimeZone)
    .getOrElse(TimeZone.getDefault)
}
