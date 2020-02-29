package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.mock._
import java.util.Locale
import i18n._
import scala.language.reflectiveCalls

class i18nSpec extends Specification with Mockito {
  "Translator" should {
    implicit val locale = Locale.ENGLISH
    def translator(result: Option[String]) = new Translator {
      var args: List[Any] = Nil
      def get(key: String, args: Any*)(implicit locale: Locale) = {
        this.args = key :: args.toList
        result
      }
    }

    "apply" in {
      "translation exists" in {
        translator(Some("test")).apply("key") mustEqual "test"
        translator(Some("message")).apply("any", "a") mustEqual "message"
      }

      "translation is missing" in {
        translator(None).apply("key") mustEqual "key"
        translator(None).apply("any", "a", 1) mustEqual "any"
      }
    }

    "field" in {
      "get translation of activerecord.models.ClassName.fieldName" in {
        val t = translator(Some("aaa"))
        t.field(classOf[models.User], "name") mustEqual "aaa"
        t.args mustEqual List("activerecord.models.User.name")
      }

      "returns titleized name when translation is missing" in {
        translator(None).field(classOf[models.User], "fieldName") mustEqual "Field Name"
      }
    }
  }

  "DefaultTranslator" should {
    "get" in {
      "English locale" in {
        DefaultTranslator.get("test.key")(Locale.ENGLISH) must beSome("test message")
      }

      "Japanese locale" in {
        DefaultTranslator.get("test.key")(Locale.JAPANESE) must beSome("テストメッセージ")
      }

      "return None if key is not exists" in {
        DefaultTranslator.get("not.exist.key")(Locale.ENGLISH) must beNone
      }

      "fallback" in {
        DefaultTranslator.get("activerecord.errors.required")(Locale.FRENCH) must beSome("is required")
      }

      "minValue validation error message" in {
        DefaultTranslator.apply("activerecord.errors.minValue", 3)(Locale.JAPANESE) mustEqual "は3以上でなければなりません"
      }
    }
  }
}
