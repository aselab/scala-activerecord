package com.github.aselab.activerecord.validations

import org.specs2.mutable._
import com.github.aselab.activerecord._
import inner._

object ValidatableSpec extends DatabaseSpecification {
  class SaveableImpl extends Saveable {
    var calledMethods = List[String]()
    override def save = {
      calledMethods :+= "save"
      true
    }
    val isNewRecord = true
  }

  case class ValidatableModel(e: Seq[String]) extends SaveableImpl with io.IO with ProductModel {
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
        m.errors must contain(exactly(
          ValidationError(mc, "", "global error1"),
          ValidationError(mc, "", "global error2"),
          ValidationError(mc, "s", "field error1"),
          ValidationError(mc, "i", "field error2")
        ))
      }

      "globalErrors" in {
        m.globalErrors must contain(exactly(
          ValidationError(mc, "", "global error1"),
          ValidationError(mc, "", "global error2")
        ))
      }

      "fieldErrors" in {
        m.fieldErrors must contain(exactly(
          ValidationError(mc, "s", "field error1"),
          ValidationError(mc, "i", "field error2")
        ))
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
        m.calledMethods mustEqual List("beforeValidation", "doValidate", "save")
      }

      "validate twice" in {
        val m = new ValidatableModel(Nil)
        m.validate must beTrue
        m.validate must beTrue
        m.calledMethods mustEqual List("beforeValidation", "doValidate")
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
        m.calledMethods mustEqual List("beforeValidation", "doValidate")
      }

      "validate twice" in {
        val m = new ValidatableModel(Seq("error"))
        m.errors.add("manual error")
        m.validate must beFalse
        m.validate must beFalse
        m.calledMethods mustEqual List("beforeValidation", "doValidate")
        m.errors.toList mustEqual List(
          ValidationError(m.getClass, "", "manual error"),
          ValidationError(m.getClass, "", "error")
        )
      }
    }
  }
}

