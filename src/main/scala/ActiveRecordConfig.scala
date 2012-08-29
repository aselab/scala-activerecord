package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.internals.DatabaseAdapter
import org.squeryl.adapters._
import java.sql.Connection
import com.jolbox.bonecp._
import com.typesafe.config._

trait ActiveRecordConfig {
  def schemaClass: String
  def connection: Connection
  def adapter: DatabaseAdapter

  def adapter(driverClass: String): DatabaseAdapter = driverClass match {
    case "org.h2.Driver" => new H2Adapter
    case "org.postgresql.Driver" => new PostgreSqlAdapter
    case "com.mysql.jdbc.Driver" => new MySQLAdapter
    case driver => throw ActiveRecordException.unsupportedDriver(driver)
  }

  def cleanup: Unit = {
    Session.cleanupResources
  }
  def translator: i18n.Translator
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
  def getString(key: String) = get[String](key).orElse(get(key, config.getString))
  def getInt(key: String) = get[Int](key).orElse(get(key, config.getInt))

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
    username.foreach(conf.setUsername(_))
    password.foreach(conf.setPassword(_))
    partitionCount.foreach(conf.setPartitionCount(_))
    maxConnectionsPerPartition.foreach(conf.setMaxConnectionsPerPartition(_))
    minConnectionsPerPartition.foreach(conf.setMinConnectionsPerPartition(_))
    new BoneCP(conf)
  }

  override def cleanup = {
    super.cleanup
    //pool.shutdown
  }

  def connection = pool.getConnection
  val translator: i18n.Translator = i18n.DefaultTranslator
}
