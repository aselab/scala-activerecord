package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.mock._
import java.util.Locale
import i18n._

object i18nSpec extends Specification with Mockito {
  "I18n" should {
    val mockTranslator = mock[Translator]
    val mockI18n = new I18n(mockTranslator)
    "translate" in {
      implicit val locale = Locale.ENGLISH
      val error = ValidationError(classOf[models.User], "name", "minLength", 4)
      mockTranslator.translateField(classOf[models.User], "name") returns "user name"
      mockTranslator.translateMessage("minLength", 4) returns "is too short"
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
        DefaultTranslator.get("activerecord.errors.required")(Locale.FRENCH) must beSome("is required")
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
