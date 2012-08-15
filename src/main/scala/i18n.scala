package com.github.aselab.activerecord

import java.util._
import java.text.MessageFormat
import mojolly.inflector.InflectorImports._

class I18n(val translator: i18n.Translator) {
  def translate(error: ValidationError)
    (implicit locale: Locale = Locale.getDefault): String =
  {
    val message = translator.translateMessage(error.message, error.args:_*)
    if (error.isGlobal) {
      message
    } else {
      translator.translateField(error.model, error.key) + " " + message
    }
  }
}

package i18n {
  trait Translator {
    def translateField(model: Class[_], name: String)(implicit locale: Locale): String
    def translateMessage(message: String, args: Any*)(implicit locale: Locale): String
  }

  object DefaultTranslator extends Translator {
    val utf8Control = new CharsetResourceBundleControl

    private val bundles = collection.mutable.Map[Locale, ResourceBundle]()
    def bundle(implicit locale: Locale) = bundles.getOrElseUpdate(locale, {
      ResourceBundle.getBundle("activerecord", locale, utf8Control)
    })

    def get(key: String)(implicit locale: Locale) = try {
      Option(bundle.getString(key))
    } catch {
      case e => None
    }

    def translateField(model: Class[_], name: String)(implicit locale: Locale) =
    {
      val key = "activerecord.models." + model.getSimpleName + "." + name
      get(key).getOrElse(name.titleize)
    }

    def translateMessage(message: String, args: Any*)(implicit locale: Locale) =
    {
      val m = get("activerecord.errors." + message).getOrElse(message)
      MessageFormat.format(m, args.map(_.asInstanceOf[AnyRef]):_*)
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
}
