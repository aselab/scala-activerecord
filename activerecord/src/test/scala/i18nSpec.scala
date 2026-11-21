package com.github.aselab.activerecord

import org.specs2.mutable._
import java.util.Locale
import i18n._

class i18nSpec extends Specification {
  "Translator" should {
    implicit val locale = Locale.ENGLISH
    class TestTranslator(result: Option[String]) extends Translator {
      var args: List[Any] = Nil
      def get(key: String, args: Any*)(implicit locale: Locale) = {
        this.args = key :: args.toList
        result
      }
    }
    def translator(result: Option[String]) = new TestTranslator(result)

    "apply" in {
      "translation exists" in {
        translator(Some("test")).apply("key") must beEqualTo("test")
        translator(Some("message")).apply("any", "a") must beEqualTo("message")
      }

      "translation is missing" in {
        translator(None).apply("key") must beEqualTo("key")
        translator(None).apply("any", "a", 1) must beEqualTo("any")
      }
    }

    "field" in {
      "get translation of activerecord.models.ClassName.fieldName" in {
        val t = translator(Some("aaa"))
        t.field(classOf[models.User], "name") must beEqualTo("aaa")
        t.args must beEqualTo(List("activerecord.models.User.name"))
      }

      "returns titleized name when translation is missing" in {
        translator(None).field(classOf[models.User], "fieldName") must beEqualTo("Field Name")
      }
    }
  }

  "DefaultTranslator" should {
    "get" in {
      "English locale" in {
        DefaultTranslator.get("test.key")(using Locale.ENGLISH) must beSome("test message")
      }

      "Japanese locale" in {
        DefaultTranslator.get("test.key")(using Locale.JAPANESE) must beSome("テストメッセージ")
      }

      "return None if key is not exists" in {
        DefaultTranslator.get("not.exist.key")(using Locale.ENGLISH) must beNone
      }

      "fallback" in {
        DefaultTranslator.get("activerecord.errors.required")(using Locale.FRENCH) must beSome("is required")
      }

      "minValue validation error message" in {
        DefaultTranslator.apply("activerecord.errors.minValue", 3)(using Locale.JAPANESE) must beEqualTo("は3以上でなければなりません")
      }
    }
  }
}
