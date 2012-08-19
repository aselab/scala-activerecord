package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.mock._
import java.util.Locale
import i18n._

object i18nSpec extends Specification with Mockito {
  "I18n" should {
    implicit val locale = Locale.ENGLISH
    val translator = mock[Translator]
    translator.translateMessage("test", "a", "b") returns "test a b"
    translator.translateField(classOf[models.User], "name") returns "user name"
    translator.translateMessage("minLength", "user name", 4) returns "user name is too short"
    val mockI18n = new I18n(translator)

    "translate global error" in {
      val error = ValidationError(classOf[models.User], "", "test", "a", "b")
      mockI18n.translate(error) mustEqual "test a b"
    }

    "translate field error" in {
      val error = ValidationError(classOf[models.User], "name", "minLength", 4)
      mockI18n.translate(error) mustEqual "user name is too short"
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
        DefaultTranslator.get("activerecord.errors.required")(Locale.FRENCH) must beSome("{0} is required")
      }
    }

    "translateField" in {
      "key exists" in {
        DefaultTranslator.translateField(classOf[models.User], "name")(Locale.JAPANESE) mustEqual "氏名"

      }

      "key is not exists" in {
        DefaultTranslator.translateField(classOf[models.User], "notExistKey")(Locale.JAPANESE) mustEqual "Not Exist Key"
      }
    }

    "translateMessage" in {
      "minValue validation error message" in {
        DefaultTranslator.translateMessage("minValue", 3)(Locale.JAPANESE) mustEqual "は3以上でなければなりません"
        
      }

      "key exists" in {
        DefaultTranslator.translateMessage("userDefined")(Locale.ENGLISH) mustEqual "user defined error message"
      }

      "key is not exists" in {
        DefaultTranslator.translateMessage("some message")(Locale.ENGLISH) mustEqual "some message"
        DefaultTranslator.translateMessage("arg is {0}", "test")(Locale.ENGLISH) mustEqual "arg is test"
      }
    }
  }
}
