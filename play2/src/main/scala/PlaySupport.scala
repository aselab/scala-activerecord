package com.github.aselab.activerecord

import java.sql.Connection
import javax.inject.Inject
import java.util.{Locale, TimeZone}
import play.api.{Configuration, Environment}
import play.api.db.DBApi
import play.api.i18n.{Messages, I18nSupport, Lang, MessagesApi}
import org.squeryl.internals.DatabaseAdapter

class PlayConfig(
  val schema: ActiveRecordTables,
  overrideSettings: Map[String, Any] = Map()
) extends ActiveRecordConfig {
  import PlayConfig._

  lazy val schemaName = schema.getClass.getName.dropRight(1)
  lazy val _prefix = configuration.getOptional[String]("schema." + schemaName).getOrElse("activerecord")

  def classLoader = environment.classLoader

  def prefix(key: String) =
    "db." + _prefix + "." + key

  private def _getString(key: String): Option[String] =
    overrideSettings.get(key).map(_.toString).orElse(
      configuration.getOptional[String](key)
    )

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

  def get[T](key: String, getter: String => Option[T]): Option[T] = {
    def inner(k: String): Option[T] = try {
      getter(k)
    } catch {
      case e: Exception => None
    }
    val k = environment.mode + "." + key
    val keyWithPrefix = prefix(k)
    val valueWithPrefix = inner(keyWithPrefix)
    debug(keyWithPrefix, valueWithPrefix)
    if (valueWithPrefix.isEmpty) {
      debug(k, inner(k))
    }
    valueWithPrefix.orElse(inner(k))
  }

  def _getString(key: String, getter: (String) => Option[String]): Option[String] = {
    val k = environment.mode + "." + key
    val keyWithPrefix = prefix(k)
    val valueWithPrefix = getter(keyWithPrefix)
    debug(keyWithPrefix, valueWithPrefix)
    if (valueWithPrefix.isEmpty) {
      debug(k, getter(k))
    }
    valueWithPrefix.orElse(getter(k))
  }

  def getString(key: String): Option[String] = get[String](key).orElse(_getString(key, configuration.getOptional[String]))
  def getInt(key: String): Option[Int] = get[Int](key).orElse(get(key, configuration.getOptional[Int]))
  def getLong(key: String): Option[Long] = get[Long](key).orElse(get(key, configuration.getOptional[Long]))
  def getBoolean(key: String): Option[Boolean] = get[Boolean](key).orElse(get(key, configuration.getOptional[Boolean]))

  def autoCreate: Boolean = getBoolean("autoCreate").getOrElse(true)

  def autoDrop: Boolean = getBoolean("autoDrop").getOrElse(false)

  def schemaClass: String =
    getString("schema").getOrElse("models.Tables")

  def connection: Connection =
    dbApi.database(_prefix).getConnection

  override def log = {
    logger.debug("----- Database setting: %s (mode: %s) -----".format(_prefix,  environment.mode))
    logger.debug("\tSchema class: " + schemaName)
    List("url" -> prefix("url"), "driver" -> prefix("driver"), "username" -> prefix("username")).foreach{case (k, v) => debug(k, Some(v))}
  }

  lazy val adapter: DatabaseAdapter = {
    adapter(_getString(prefix("driver")).orElse(getString("driver")).getOrElse("org.h2.Driver"))
  }

  def translator: i18n.Translator = PlayTranslator
}

object PlayTranslator extends i18n.Translator  {
  def get(key: String, args: Any*)(implicit locale: Locale): Option[String] = {
    implicit val lang = Lang(locale.getLanguage, locale.getCountry)

    if (PlayConfig.messages.messages.get(lang.code).exists(_.isDefinedAt(key))) {
      Some(PlayConfig.messages(key, args:_*))
    } else {
      i18n.DefaultTranslator.get(key, args:_*)
    }
  }
}

trait PlaySupport { self: ActiveRecordTables =>
  override def loadConfig(c: Map[String, Any]): ActiveRecordConfig =
    new PlayConfig(self, c)
}

object PlayConfig {
  @Inject
  var messages: MessagesApi = _

  @Inject
  var configuration: Configuration = _

  @Inject
  var environment: Environment = _

  @Inject
  var dbApi: DBApi = _

  def loadSchemas = configuration.getOptional[Configuration]("schema")
    .map(_.keys).getOrElse(List("models.Tables")).map(ActiveRecordTables.find).toSeq
}
