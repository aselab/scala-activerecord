package com.github.aselab.activerecord.i18n

import java.util._
import java.text.MessageFormat
import mojolly.inflector.InflectorImports._

trait Translator {
  def apply(key: String, args: Any*)(implicit locale: Locale): String =
    get(key, args:_*).getOrElse(key)
  
  def get(key: String, args: Any*)(implicit locale: Locale): Option[String]

  def field(model: Class[_], name: String)(implicit locale: Locale): String = {
    val key = "activerecord.models." + model.getSimpleName + "." + name
    get(key).getOrElse(name.titleize)
  }

  def errorMessage(error: String, args: Any*)(implicit locale: Locale): String
    = get("activerecord.errors." + error, args:_*).
      getOrElse(apply(error, args:_*))
}

object DefaultTranslator extends Translator {
  val utf8Control = new CharsetResourceBundleControl

  def bundle(implicit locale: Locale) = 
    ResourceBundle.getBundle("activerecord", locale, utf8Control)

  def get(key: String, args: Any*)(implicit locale: Locale) = try {
    Option(bundle.getString(key)).map(msg =>
      MessageFormat.format(msg, args.map(_.asInstanceOf[AnyRef]):_*)
    )
  } catch {
    case e => None
  }
}

class CharsetResourceBundleControl(charset: String = "UTF8") extends ResourceBundle.Control {
  override def newBundle(baseName: String, locale: Locale, format: String, loader: ClassLoader, reload: Boolean): ResourceBundle = {
    val bundleName = toBundleName(baseName, locale)
    val resourceName = toResourceName(bundleName, "properties")
    
    (if (reload) {
      Option(loader.getResource(resourceName)) map {_.openConnection} map {conn =>
        conn.setUseCaches(false)
        conn.getInputStream
      }
    } else {
      Option(loader.getResourceAsStream(resourceName))
    }) map { stream =>
      val bundle = new PropertyResourceBundle(new java.io.InputStreamReader(stream, charset))
      stream.close
      bundle
    } match {
      case Some(bundle) => bundle
      case None => null
    }
  }
}
