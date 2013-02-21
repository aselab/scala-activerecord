package com.github.aselab.activerecord.validations

import org.specs2.mutable._
import com.github.aselab.activerecord._
import inner._

object ValidatableSpec extends ActiveRecordSpecification {
  class SaveableImpl extends Saveable {
    var calledMethods = List[String]()
    override def save = {
      calledMethods :+= "save"
      true
    }
    val isNewRecord = true
  }

  case class ValidatableModel(e: Seq[String]) extends SaveableImpl with Validatable {
    override def doValidate {
      e.foreach(errors.add)
      calledMethods :+= "doValidate"
    }

    override def beforeValidation() {
      calledMethods :+= "beforeValidation"
    }
  }

  "Validatable" should {
    "addError" in {
      val m = ValidatableModel(Nil)
      m.errors.add("global error1")
      m.errors.add("global error2")
      m.errors.add("s", "field error1")
      m.errors.add("i", "field error2")
      val mc = m.getClass

      "errors" in {
        m.errors must contain(
          ValidationError(mc, "", "global error1"),
          ValidationError(mc, "", "global error2"),
          ValidationError(mc, "s", "field error1"),
          ValidationError(mc, "i", "field error2")
        ).only
      }

      "globalErrors" in {
        m.globalErrors must contain(
          ValidationError(mc, "", "global error1"),
          ValidationError(mc, "", "global error2")
        ).only
      }

      "fieldErrors" in {
        m.fieldErrors must contain(
          ValidationError(mc, "s", "field error1"),
          ValidationError(mc, "i", "field error2")
        ).only
      }

      "hasErrors" in {
        m.hasErrors must beTrue
      }

      "hasError" in {
        m.hasError("s") must beTrue
        m.hasError("i") must beTrue
        m.hasError("xxx") must beFalse
      }

      "isValid" in {
        m.isValid must beFalse
      }
    }

    "validate success" in {
      "validate" in {
        val m = new ValidatableModel(Nil)
        m.validate must beTrue
      }

      "save" in {
        val m = new ValidatableModel(Nil)
        m.save must beTrue
        m.calledMethods must contain("beforeValidation", "doValidate", "save").only
      }
    }

    "validate failure" in {
      "validate" in {
        val m = new ValidatableModel(Seq("error"))
        m.validate must beFalse
      }

      "save" in {
        val m = new ValidatableModel(Seq("error"))
        m.save must beFalse
        m.calledMethods must contain("beforeValidation", "doValidate").only
      }
    }
  }
}

