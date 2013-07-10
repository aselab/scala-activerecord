package com.github.aselab.activerecord

import java.sql.Connection
import org.squeryl.internals.DatabaseAdapter
import com.typesafe.config._
import play.api.Play.current
import java.util.{Locale, TimeZone}

class PlayConfig(
  schema: ActiveRecordTables,
  overrideSettings: Map[String, Any] = Map()
) extends ActiveRecordConfig {
  lazy val schemaName = schema.getClass.getName.dropRight(1)
  lazy val _prefix = current.configuration.getString("schema." + schemaName).getOrElse("activerecord")

  def prefix(key: String) =
    "db." + _prefix + "." + key

  def classLoader = play.api.Play.application.classloader

  def getString(key: String): Option[String] =
    overrideSettings.get(prefix(key)).map(_.toString).orElse(
      current.configuration.getString(prefix(key))
    )

  private def debug(key: String): Unit =
    debug(key, current.configuration.getString(key))

  def connection: Connection =
    play.api.db.DB.getConnection(_prefix)

  override def log = {
    logger.debug("----- Database setting: %s (mode: %s) -----".format(_prefix,  play.api.Play.application.mode))
    logger.debug("\tSchema class: " + schemaName)
    List(prefix("url"), prefix("driver"), prefix("user")).foreach(debug)
  }

  lazy val adapter: DatabaseAdapter = {
    adapter(getString(prefix("driver")).orElse(getString("driver")).getOrElse("org.h2.Driver"))
  }

  def translator: i18n.Translator = PlayTranslator

  def timeZone: TimeZone = TimeZone.getDefault
}

object PlayTranslator extends i18n.Translator {
  import play.api.i18n._

  def get(key: String, args: Any*)(implicit locale: Locale):Option[String] = {
    implicit val lang = Lang(locale.getLanguage)
    if (Messages.messages.get(lang.code).exists(_.isDefinedAt(key))) {
      Some(Messages(key, args:_*))
    } else {
      i18n.DefaultTranslator.get(key, args:_*)
    }
  }
}

trait PlaySupport { self: ActiveRecordTables =>
  override def loadConfig(c: Map[String, Any]): ActiveRecordConfig =
    new PlayConfig(self, c)
}
