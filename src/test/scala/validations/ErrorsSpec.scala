package com.github.aselab.activerecord.validations

import org.specs2.mutable._
import org.specs2.mock._

import java.util.Locale
import com.github.aselab.activerecord._

object ErrorsSpec extends ActiveRecordSpecification with Mockito {
  val userClass = classOf[models.User]

  "Errors" should {
    "add message only" in {
      val errors = new Errors(userClass)
      errors.add("error message")
      val error = ValidationError(userClass, "", "error message")

      "iterator contains added errors" in {
        errors must contain(error)
      }

      "global contains added errors" in {
        errors.global must contain(error)
      }
    }

    "add message with fieldName" in {
      val errors = new Errors(userClass)
      errors.add("name", "required")
      errors.add("name", "minLength", 10)

      val requiredError = ValidationError(userClass, "name", "required")
      val minLengthError = ValidationError(userClass, "name", "minLength", 10)

      "iterator contains added errors" in {
        errors must contain(requiredError, minLengthError)
      }

      "exists(fieldName) returns whether the error exists or not" in {
        errors.exists("name") must beTrue
        errors.exists("other") must beFalse
      }

      "get(fieldName) returns errors on fieldName" in {
        errors.get("name") must contain(requiredError, minLengthError)
        errors.get("other") must beEmpty
      }

      "apply is the same as get" in {
        errors("name") must contain(requiredError, minLengthError)
        errors("other") must beEmpty
      }

      "global does not contain added errors" in {
        errors.global must beEmpty
      }
    }

    "clear" in {
      val errors = new Errors(userClass)
      errors.add("some message")
      errors.add("name", "required")

      errors.global.size mustEqual 1
      errors("name").size mustEqual 1
      errors.size mustEqual 2
      errors.clear
      errors.global must beEmpty
      errors("name") must beEmpty
      errors must beEmpty
    }

    "messages" in {
      val messagesEn = Seq("message1", "message2")
      val messagesJa = Seq("メッセージ1", "メッセージ2")
      val mockErrors = (messagesEn, messagesJa).zipped.map {case (en, ja) =>
        val e = mock[ValidationError]
        e.translation(Locale.ENGLISH) returns en
        e.translation(Locale.JAPANESE) returns ja
        e
      }
      val errors = new Errors(userClass) {
        override def iterator = mockErrors.iterator
      }

      "locale en" in {
        errors.messages(Locale.ENGLISH) mustEqual messagesEn
      }

      "locale ja" in {
        errors.messages(Locale.JAPANESE) mustEqual messagesJa
      }
    }
  }
}
