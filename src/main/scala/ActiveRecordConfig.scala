package com.github.aselab.activerecord

import org.squeryl.Session
import org.squeryl.internals.DatabaseAdapter
import org.squeryl.adapters._
import java.sql.Connection
import java.util.TimeZone
import com.jolbox.bonecp._
import com.typesafe.config._
import org.slf4j.{Logger, LoggerFactory}

object Config {
  private var _conf: ActiveRecordConfig = _
  private var _timeZone: TimeZone = _

  def confOption: Option[ActiveRecordConfig] = Option(_conf)
  def conf: ActiveRecordConfig = confOption.getOrElse(throw ActiveRecordException.notInitialized)
  def conf_=(value: ActiveRecordConfig): Unit = _conf = value

  def schema: ActiveRecordTables = conf.schema
  def connection: java.sql.Connection = conf.connection
  def adapter: DatabaseAdapter = conf.adapter

  def cleanup: Unit = conf.cleanup

  def translator: i18n.Translator =
    confOption.map(_.translator).getOrElse(i18n.DefaultTranslator)

  def timeZone: TimeZone = Option(_timeZone).getOrElse(conf.timeZone)
  def timeZone_=(value: TimeZone): Unit = _timeZone = value
  def logger: Logger = conf.logger
}

trait ActiveRecordConfig {
  def schemaClass: String
  def connection: Connection
  def adapter: DatabaseAdapter
  lazy val schema = try {
    reflections.ReflectionUtil.classToCompanion(schemaClass)
      .asInstanceOf[ActiveRecordTables]
  } catch {
    case e => throw ActiveRecordException.cannotLoadSchema(schemaClass)
  }

  def adapter(driverClass: String): DatabaseAdapter = driverClass match {
    case "org.h2.Driver" => new H2Adapter
    case "org.postgresql.Driver" => new PostgreSqlAdapter
    case "com.mysql.jdbc.Driver" => new MySQLAdapter
    case "oracle.jdbc.OracleDriver" => new OracleAdapter
    case "org.apache.derby.jdbc.EmbeddedDriver" => new DerbyAdapter
    case driver => throw ActiveRecordException.unsupportedDriver(driver)
  }

  def cleanup: Unit = {
    Session.cleanupResources
  }
  def translator: i18n.Translator
  def timeZone: TimeZone

  val logger = LoggerFactory.getLogger("activerecord")
}

class DefaultConfig(
  config: Config = ConfigFactory.load(),
  overrideSettings: Map[String, Any] = Map()
) extends ActiveRecordConfig {
  val env = System.getProperty("run.mode", "dev")

  def get[T](key: String): Option[T] = overrideSettings.get(key).map(_.asInstanceOf[T])
  def get[T](key: String, getter: String => T): Option[T] = try {
    Option(getter(env + "." + key))
  } catch {
    case e: ConfigException.Missing => None
  }
  def getString(key: String): Option[String] = get[String](key).orElse(get(key, config.getString))
  def getInt(key: String): Option[Int] = get[Int](key).orElse(get(key, config.getInt))

  lazy val schemaClass = getString("schema").getOrElse("models.Tables")
  lazy val driverClass = getString("driver").getOrElse("org.h2.Driver")
  lazy val jdbcurl = getString("jdbcurl").getOrElse("jdbc:h2:mem:activerecord")
  lazy val username = getString("username")
  lazy val password = getString("password")
  lazy val partitionCount = getInt("partitionCount")
  lazy val maxConnectionsPerPartition = getInt("maxConnectionsPerPartition")
  lazy val minConnectionsPerPartition = getInt("minConnectionsPerPartition")

  lazy val adapter: DatabaseAdapter = adapter(driverClass)

  lazy val pool = {
    try {
      Class.forName(driverClass)
    } catch {
      case e => throw ActiveRecordException.missingDriver(driverClass)
    }

    val conf = new BoneCPConfig
    conf.setJdbcUrl(jdbcurl)
    username.foreach(conf.setUsername)
    password.foreach(conf.setPassword)
    partitionCount.foreach(conf.setPartitionCount)
    maxConnectionsPerPartition.foreach(conf.setMaxConnectionsPerPartition)
    minConnectionsPerPartition.foreach(conf.setMinConnectionsPerPartition)
    new BoneCP(conf)
  }

  override def cleanup: Unit = {
    super.cleanup
  }

  def connection: Connection = pool.getConnection
  val translator: i18n.Translator = i18n.DefaultTranslator
  lazy val timeZone = getString("timeZone").map(TimeZone.getTimeZone)
    .getOrElse(TimeZone.getDefault)
}
