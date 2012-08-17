package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.mock._

object ValidatorSpec extends Specification with Mockito {
  import annotation.target._
  type CustomAnnotation = sample.CustomAnnotation @field

  val customValidator =  new Validator[sample.CustomAnnotation] {
    def validate(value: Any) =
      if (value != annotation.value) errors.add(fieldName, "custom")
  }

  def mockAnnotation[T <: Validator.AnnotationType](
    message: String = "", on: String = "save"
  )(implicit m: Manifest[T]) = {
    val a = mock[T]
    a.message returns message
    a.on returns on
    a
  }

  def validate[A <: Validator.AnnotationType, T <: Validator[A]](validator: T, a: A, m: Model) =
    validator.validateWith(m.value, a, m, "value")

  case class Model(value: Any, isNewInstance: Boolean = true) extends Validatable
  val modelClass = classOf[Model]

  "Validator" should {
    "be able to register and unregister custom validator" in {
      val c = classOf[sample.CustomAnnotation]

      Validator.get(c) must beNone
      customValidator.register
      Validator.get(c) must beSome(customValidator)
      customValidator.unregister
      Validator.get(c) must beNone
    }

    "validateWith" in {
      "on save" in {
        val a = mockAnnotation[sample.CustomAnnotation](on="save")
        a.value returns "match"

        val onCreate = Model("", true)
        validate(customValidator, a, onCreate)
        onCreate.errors must not beEmpty

        val onUpdate = Model("", false)
        validate(customValidator, a, onUpdate)
        onUpdate.errors must not beEmpty
      }
      
      "on create" in {
        val a = mockAnnotation[sample.CustomAnnotation](on="create")
        a.value returns "match"

        val onCreate = Model("", true)
        validate(customValidator, a, onCreate)
        onCreate.errors must not beEmpty

        val onUpdate = Model("", false)
        validate(customValidator, a, onUpdate)
        onUpdate.errors must beEmpty
      }
      
      "on update" in {
        val a = mockAnnotation[sample.CustomAnnotation](on="update")
        a.value returns "match"

        val onCreate = Model("", true)
        validate(customValidator, a, onCreate)
        onCreate.errors must beEmpty

        val onUpdate = Model("", false)
        validate(customValidator, a, onUpdate)
        onUpdate.errors must not beEmpty
      }
    }

    "requiredValidator" in {
      val validator = Validator.requiredValidator
      val a = mockAnnotation[annotations.Required]()
        
      "invalid if value is null" in {
        val m = Model(null)
        validate(validator, a, m)
        m.errors must contain(ValidationError(modelClass, "value", "required"))
      }

      "invalid if value is empty string" in {
        val m = Model("")
        validate(validator, a, m)
        m.errors must contain(ValidationError(modelClass, "value", "required"))
      }

      "valid if value is not empty" in {
        val m = Model("a")
        validate(validator, a, m)
        m.errors must beEmpty
      }

      "annotation message" in {
        val a = mockAnnotation[annotations.Required](message="test")
        val m = Model("")
        validate(validator, a, m)
        m.errors must contain(ValidationError(modelClass, "value", "test"))
      }
    }

  }
}

