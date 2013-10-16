package com.github.aselab.activerecord.scalatra

import com.github.aselab.activerecord._
import com.typesafe.config._
import org.scalatra.i18n.Messages
import java.util.Locale
import java.text.MessageFormat

trait ScalatraSupport { self: ActiveRecordTables =>
  override def loadConfig(config: Map[String, Any]): ActiveRecordConfig =
    new ScalatraConfig(overrideSettings = config)
}

class ScalatraConfig(
  config: Config = ConfigFactory.load(),
  overrideSettings: Map[String, Any] = Map()
) extends DefaultConfig(config, overrideSettings) {
  override val translator = ScalatraTranslator
}

object ScalatraTranslator extends i18n.Translator {
  val _messages = collection.mutable.Map[Locale, Messages]()
  def get(key: String, args: Any*)(implicit locale: Locale):Option[String] = {
    val messages = _messages.getOrElseUpdate(locale, new Messages(locale))
    messages.get(key).map(msg =>
      MessageFormat.format(msg, args.map(_.asInstanceOf[AnyRef]):_*)
    ).orElse(i18n.DefaultTranslator.get(key, args:_*))
  }
}
