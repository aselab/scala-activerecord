package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._

import java.util.{Date, UUID}
import java.sql.Timestamp

object ValidationSupportSpec extends ActiveRecordSpecification {
  import dsl._
  import annotation.target._
  type CustomAnnotation = sample.CustomAnnotation @field

  case class CustomAnnotationModel(
    @CustomAnnotation("match") value: String
  ) extends ActiveRecord

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

  case class UserModel(
    @Transient @Confirm var password: String,
    @Transient var passwordConfirmation: String
  ) extends ActiveRecord

  case class MissingConfirmField(
    @Confirm test: String
  ) extends ActiveRecord

  case class ValidationSupportModel(
    @Email email: String = ""
  ) extends ProductModel with ValidationSupport {
    val isNewInstance = true
  }

  case class AnnotationOptionModel(
    @Required(message="custom message") message: String = "a",
    @Required(on="save") onSave: String = "a",
    @Required(on="create") onCreate: String = "a",
    @Required(on="update") onUpdate: String = "a",
    persisted: Boolean = false
  ) extends ActiveRecord {
    override def isNewInstance =  !persisted
  }

  object CustomAnnotationModel extends ActiveRecordCompanion[CustomAnnotationModel]
  object ValidationModel extends ActiveRecordCompanion[ValidationModel]
  object UserModel extends ActiveRecordCompanion[UserModel]

  object MissingConfirmField extends ActiveRecordCompanion[MissingConfirmField]

  object ValidationSupportModel extends ProductModelCompanion[ValidationSupportModel]

  object AnnotationOptionModel extends ActiveRecordCompanion[AnnotationOptionModel]

  "ValidationSupport" should {
    "doValidate" in {
      "with custom annotation" in {
        val customValidator =  new Validator[sample.CustomAnnotation]{
          def validate(value: Any) =
            if (value != annotation.value) errors.add(fieldName, "custom")
        }.register

        val c = classOf[CustomAnnotationModel]
        val m1 = CustomAnnotationModel("not match")
        val m2 = CustomAnnotationModel("match")
        m1.validate must beFalse
        m2.validate must beTrue

        customValidator.unregister

        m1.errors must contain(ValidationError(c, "value", "custom"))
        m2.errors must beEmpty
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

    "annotation options" in {
      val c = classOf[AnnotationOptionModel]

      "message" in {
        val m = AnnotationOptionModel(message = "")
        m.validate must beFalse
        m.errors must contain(ValidationError(c, "message", "custom message"))
      }

      "on save" in {
        val onCreate = AnnotationOptionModel(onSave = "")
        onCreate.validate must beFalse
        onCreate.errors must not beEmpty

        val onUpdate = AnnotationOptionModel(onSave = "", persisted = true)
        onUpdate.validate must beFalse
        onUpdate.errors must not beEmpty
      }

      "on create" in {
        val onCreate = AnnotationOptionModel(onCreate = "")
        onCreate.validate must beFalse
        onCreate.errors must not beEmpty

        val onUpdate = AnnotationOptionModel(onCreate = "", persisted = true)
        onUpdate.validate must beTrue
        onUpdate.errors must beEmpty
      }

      "on update" in {
        val onCreate = AnnotationOptionModel(onUpdate = "")
        onCreate.validate must beTrue
        onCreate.errors must beEmpty

        val onUpdate = AnnotationOptionModel(onUpdate = "", persisted = true)
        onUpdate.validate must beFalse
        onUpdate.errors must not beEmpty
      }
    }

    "validate on save" in {
      "not extends ActiveRecord" in {
        val v = ValidationSupportModel("aaa")
        v.save must beFalse
        v.errors must not beEmpty
      }

      "extends ActiveRecord" in {
        val m = UserModel("a", "b")
        m.save must beFalse
        m.errors must not beEmpty
      }
    }
  }
}

