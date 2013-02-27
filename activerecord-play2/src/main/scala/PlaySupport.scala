package com.github.aselab.activerecord

import java.sql.Connection
import org.squeryl.internals.DatabaseAdapter
import com.typesafe.config._
import play.api.Play.current
import java.util.{Locale, TimeZone}

class PlayConfig(
  overrideSettings: Map[String, Any] = Map()
) extends ActiveRecordConfig {
  def getString(key: String, default: String): String =
    overrideSettings.get(key).map(_.toString).orElse(
      current.configuration.getString(key)
    ).getOrElse(default)

  def schemaClass: String =
    getString("activerecord.schema", "models.Tables")

  def connection: Connection =
    play.api.db.DB.getConnection("activerecord")

  lazy val adapter: DatabaseAdapter =
    adapter(getString("db.activerecord.driver", "org.h2.Driver"))

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
  override def loadConfig(config: Map[String, Any]): ActiveRecordConfig =
    new PlayConfig(config)
}
