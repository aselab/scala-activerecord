package com.github.aselab.activerecord

import java.sql.Connection
import org.squeryl.internals.DatabaseAdapter
import play.api.Play.current
import java.util.{Locale, TimeZone}
import play.api.i18n.{Messages, I18nSupport}


class PlayConfig(
  val schema: ActiveRecordTables,
  overrideSettings: Map[String, Any] = Map()
) extends ActiveRecordConfig {
  lazy val schemaName = schema.getClass.getName.dropRight(1)
  lazy val _prefix = current.configuration.getString("schema." + schemaName).getOrElse("activerecord")

  def prefix(key: String) =
    "db." + _prefix + "." + key

  def classLoader = play.api.Play.application.classloader

  private def debug(key: String): Unit =
    debug(key, current.configuration.getString(key))

  private def _getString(key: String): Option[String] =
    overrideSettings.get(key).map(_.toString).orElse(
      current.configuration.getString(key)
    )

  def getString(key: String): Option[String] = _getString("activerecord." + key)

  def getBoolean(key: String, default: Boolean): Boolean =
    overrideSettings.get(key).map(_.asInstanceOf[Boolean]).orElse(
      current.configuration.getBoolean(key)
    ).getOrElse(default)

  def autoCreate: Boolean = getBoolean("activerecord.autoCreate", true)

  def autoDrop: Boolean = getBoolean("activerecord.autoDrop", false)

  def schemaClass: String =
    getString("schema").getOrElse("models.Tables")

  def connection: Connection =
    play.api.db.DB.getConnection(_prefix)

  override def log = {
    logger.debug("----- Database setting: %s (mode: %s) -----".format(_prefix,  play.api.Play.application.mode))
    logger.debug("\tSchema class: " + schemaName)
    List(prefix("url"), prefix("driver"), prefix("user")).foreach(debug)
  }

  lazy val adapter: DatabaseAdapter = {
    adapter(_getString(prefix("driver")).orElse(getString("driver")).getOrElse("org.h2.Driver"))
  }

  def translator: i18n.Translator = PlayTranslator
}

object PlayTranslator extends i18n.Translator  {
  import play.api.i18n._
  import play.api.i18n.Messages.Implicits._

  def get(key: String, args: Any*)(implicit locale: Locale):Option[String] = {
    implicit val lang = Lang(locale.getLanguage, locale.getCountry)
    val messages = implicitly[Messages]

    if (messages.isDefinedAt(key)) {
      Some(messages(key))
    } else {
      implicit val locale = messages.lang.toLocale
      i18n.DefaultTranslator.get(key, args:_*)
    }
  }
}

trait PlaySupport { self: ActiveRecordTables =>
  override def loadConfig(c: Map[String, Any]): ActiveRecordConfig =
    new PlayConfig(self, c)
}

object PlayConfig {
  def loadSchemas = current.configuration.getConfig("schema")
    .map(_.keys).getOrElse(List("models.Tables")).map(ActiveRecordTables.find).toSeq
}
