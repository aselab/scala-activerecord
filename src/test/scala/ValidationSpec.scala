package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._
import dsl._

import java.util.{Date, UUID}
import java.sql.Timestamp

object ValidationSpec extends ActiveRecordSpecification {
  class SaveableImpl extends Saveable {
    var calledMethods = List[String]()
    override def save = {
      calledMethods :+= "save"
      true
    }
  }

  case class ValidatableModel(e: Seq[String]) extends SaveableImpl with Validatable {
    override def doValidate {
      e.foreach(errors.add(_))
      calledMethods :+= "doValidate"
    }

    override def beforeValidation() {
      calledMethods :+= "beforeValidation"
    }
  }

  case class Dummy(@Unique s1: String, @Required s2: String) extends ActiveRecord

  object Dummy extends ActiveRecordCompanion[Dummy]

  case class ValidationModel(
    @Length(min=3, max=10) length: String = "aaaaa",
    @Range(max=5.3) maxValue: Double = 0,
    @Range(min=0) minValue: Long = 1,
    @Range(min = 5, max = 10) range: Int = 7,
    @Checked checked: Boolean = true,
    @Email email: String = "test@example.com",
    @Format("""\d+""") format: String = "100",
    @Length(min=3, max=10) lengthOption: Option[String] = Some("aaaaa"),
    @Range(max=5.3) maxValueOption: Option[Double] = Some(0),
    @Range(min=0) minValueOption: Option[Long] = Some(1),
    @Range(min = 5, max = 10) rangeOption: Option[Int] = Some(7),
    @Checked checkedOption: Option[Boolean] = Some(true),
    @Email emailOption: Option[String] = Some("test@example.com"),
    @Format("""\d+""") formatOption: Option[String] = Some("100")
  ) extends ActiveRecord

  object ValidationModel extends ActiveRecordCompanion[ValidationModel]

  case class UserModel(
    @Transient @Confirm var password: String,
    @Transient var passwordConfirmation: String
  ) extends ActiveRecord

  object UserModel extends ActiveRecordCompanion[UserModel]

  case class MissingConfirmField(
    @Confirm test: String
  ) extends ActiveRecord
  object MissingConfirmField extends ActiveRecordCompanion[MissingConfirmField]

  case class ValidateModel(
    @Email email: String = ""
  ) extends ProductModel

  object ValidateModel extends ProductModelCompanion[ValidateModel]

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

    "Validator" in {
      val dummyValidator =  new Validator[annotations.Unique]{
        def validate(value: Any) =
          if (value.toString == "dummy") errors.add(fieldName, "dummy")
      }.register

      val dummyValidator2 = new Validator[annotations.Required]{
        def validate(value: Any) =
          if (value.toString == "dummy2") errors.add(fieldName, "dummy2")
      }.register

      "get" in {
        Validator.get(classOf[annotations.Unique]) must beSome(dummyValidator)
        Validator.get(classOf[annotations.Required]) must beSome(dummyValidator2)
      }

      "doValidate" in {
        "add custom annotations" in {
          val c = classOf[Dummy]
          val m1 = Dummy("dummy", "")
          val m2 = Dummy("", "dummy2")
          val m3 = Dummy("dummy", "dummy2")
          m1.validate
          m2.validate
          m3.validate
          m1.errors must contain(ValidationError(c, "s1", "dummy"))
          m2.errors must contain(ValidationError(c, "s2", "dummy2"))
          m3.errors must contain(ValidationError(c, "s1", "dummy"), ValidationError(c, "s2", "dummy2"))
        }

        "@Confirm" in {
          val c = classOf[UserModel]
          
          "not equals confirmation field" in {
            val m = UserModel("aaa", "bbb")
            m.validate must beFalse
            m.errors must contain(ValidationError(c, "password", "confirmation"))
          }

          "equals confirmation field" in {
            val m = UserModel("aaa", "aaa")
            m.validate must beTrue
            m.errors must beEmpty
          }

          "throws exception when confirmation field is not defined" in {
            val m = MissingConfirmField("aaa")
            m.validate must throwA(ActiveRecordException.notfoundConfirmField("testConfirmation"))
          }
        }

        "@Length" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(length = "")
          val m2 = ValidationModel(length = "a" * 5)
          val m3 = ValidationModel(length = "a" * 11)
          val m4 = ValidationModel(length = null)
          m1.validate
          m2.validate
          m3.validate
          m4.validate
          m1.errors must contain(ValidationError(c, "length", "minLength", 3))
          m2.errors must beEmpty
          m3.errors must contain(ValidationError(c, "length", "maxLength", 10))
          m4.errors must contain(ValidationError(c, "length", "minLength", 3))
        }

        "@Length (Option)" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(lengthOption = Some(""))
          val m2 = ValidationModel(lengthOption = Some("a" * 5))
          val m3 = ValidationModel(lengthOption = Some("a" * 11))
          val m4 = ValidationModel(lengthOption = None)
          val m5 = ValidationModel(lengthOption = Some(null))
          m1.validate
          m2.validate
          m3.validate
          m4.validate
          m5.validate
          m1.errors must contain(ValidationError(c, "lengthOption", "minLength", 3))
          m2.errors must beEmpty
          m3.errors must contain(ValidationError(c, "lengthOption", "maxLength", 10))
          m4.errors must beEmpty
          m5.errors must contain(ValidationError(c, "lengthOption", "minLength", 3))
        }

        "@Range max" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(maxValue = 5)
          val m2 = ValidationModel(maxValue = 4)
          val m3 = ValidationModel(maxValue = 6)
          m1.validate
          m2.validate
          m3.validate
          m1.errors must beEmpty
          m2.errors must beEmpty
          m3.errors must contain(ValidationError(c, "maxValue", "maxValue", 5.3))
        }

       "@Range min" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(minValue = 0)
          val m2 = ValidationModel(minValue = 1)
          val m3 = ValidationModel(minValue = -1)
          m1.validate
          m2.validate
          m3.validate
          m1.errors must beEmpty
          m2.errors must beEmpty
          m3.errors must contain(ValidationError(c, "minValue", "minValue", 0))
        }

       "@Range" in {
          val c = classOf[ValidationModel]
          val models = List(
            ValidationModel(range = 4),
            ValidationModel(range = 5),
            ValidationModel(range = 6),
            ValidationModel(range = 9),
            ValidationModel(range = 10),
            ValidationModel(range = 11))
          models.foreach(_.validate)
          models.map(_.errors.toList) must equalTo(List(
            List(ValidationError(c, "range", "minValue", 5)),
            Nil,
            Nil,
            Nil,
            Nil,
            List(ValidationError(c, "range", "maxValue", 10))
          ))
        }

        "@Range max (Option)" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(maxValueOption = Some(5))
          val m2 = ValidationModel(maxValueOption = Some(4))
          val m3 = ValidationModel(maxValueOption = Some(6))
          val m4 = ValidationModel(maxValueOption = None)
          m1.validate
          m2.validate
          m3.validate
          m4.validate
          m1.errors must beEmpty
          m2.errors must beEmpty
          m3.errors must contain(ValidationError(c, "maxValueOption", "maxValue", 5.3))
          m4.errors must beEmpty
        }

       "@Range min (Option)" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(minValueOption = Some(0))
          val m2 = ValidationModel(minValueOption = Some(1))
          val m3 = ValidationModel(minValueOption = Some(-1))
          val m4 = ValidationModel(minValueOption = None)
          m1.validate
          m2.validate
          m3.validate
          m4.validate
          m1.errors must beEmpty
          m2.errors must beEmpty
          m3.errors must contain(ValidationError(c, "minValueOption", "minValue", 0))
          m4.errors must beEmpty
        }

       "@Range (Option)" in {
          val c = classOf[ValidationModel]
          val models = List(
            ValidationModel(rangeOption = Some(4)),
            ValidationModel(rangeOption = Some(5)),
            ValidationModel(rangeOption = Some(6)),
            ValidationModel(rangeOption = Some(9)),
            ValidationModel(rangeOption = Some(10)),
            ValidationModel(rangeOption = Some(11)),
            ValidationModel(rangeOption = None)
          )
          models.foreach(_.validate)
          models.map(_.errors.toList) must equalTo(List(
            List(ValidationError(c, "rangeOption", "minValue", 5)),
            Nil,
            Nil,
            Nil,
            Nil,
            List(ValidationError(c, "rangeOption", "maxValue", 10)),
            Nil
          ))
        }

        "@Checked" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(checked = true)
          val m2 = ValidationModel(checked = false)
          m1.validate
          m2.validate
          m1.errors must beEmpty
          m2.errors must contain(ValidationError(c, "checked", "checked"))
        }

        "@Checked (Option)" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(checkedOption = Some(true))
          val m2 = ValidationModel(checkedOption = Some(false))
          val m3 = ValidationModel(checkedOption = None)
          m1.validate
          m2.validate
          m1.errors must beEmpty
          m2.errors must contain(ValidationError(c, "checkedOption", "checked"))
          m3.errors must beEmpty
        }

        "@Email" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(email = "test@example.com")
          val m2 = ValidationModel(email = "aaa")
          val m3 = ValidationModel(email = null)
          m1.validate
          m2.validate
          m3.validate
          m1.errors must beEmpty
          m2.errors must contain(ValidationError(c, "email", "invalid"))
          m3.errors must beEmpty
        }

        "@Email (Option)" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(emailOption = Some("test@example.com"))
          val m2 = ValidationModel(emailOption = Some("aaa"))
          val m3 = ValidationModel(emailOption = None)
          val m4 = ValidationModel(emailOption = Some(null))
          m1.validate
          m2.validate
          m3.validate
          m4.validate
          m1.errors must beEmpty
          m2.errors must contain(ValidationError(c, "emailOption", "invalid"))
          m3.errors must beEmpty
          m4.errors must beEmpty
        }

        "@Format" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(format = "200")
          val m2 = ValidationModel(format = "aaa")
          val m3 = ValidationModel(format = null)
          m1.validate
          m2.validate
          m3.validate
          m1.errors must beEmpty
          m2.errors must contain(ValidationError(c, "format", "format"))
          m3.errors must beEmpty
        }

        "@Format (Option)" in {
          val c = classOf[ValidationModel]
          val m1 = ValidationModel(formatOption = Some("200"))
          val m2 = ValidationModel(formatOption = Some("aaa"))
          val m3 = ValidationModel(formatOption = None)
          val m4 = ValidationModel(formatOption = Some(null))
          m1.validate
          m2.validate
          m3.validate
          m4.validate
          m1.errors must beEmpty
          m2.errors must contain(ValidationError(c, "formatOption", "format"))
          m3.errors must beEmpty
          m4.errors must beEmpty
        }
      }

      "unregister" in {
        dummyValidator.unregister
        Validator.get(classOf[annotations.Required]) must beSome(dummyValidator2)
        Validator.get(classOf[annotations.Unique]) must beNone
      }

      "not extends ActivevRecord" in {
        val v = ValidateModel("aaa")
        v.validate
        v.errors must not beEmpty
      }
    }
  }
}

