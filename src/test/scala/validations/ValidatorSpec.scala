package com.github.aselab.activerecord

import org.specs2.mutable._

object ValidatorSpec extends Specification {
  import annotation.target._
  type CustomAnnotation = sample.CustomAnnotation @field

  val customValidator =  new Validator[sample.CustomAnnotation] {
    def validate(value: Any) =
      if (value != annotation.value) errors.add(fieldName, "custom")
  }

  "Validator" should {
    "be able to register and unregister custom validator" in {
      val c = classOf[sample.CustomAnnotation]

      Validator.get(c) must beNone
      customValidator.register
      Validator.get(c) must beSome(customValidator)
      customValidator.unregister
      Validator.get(c) must beNone
    }
  }
}

