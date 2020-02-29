package com.github.aselab.activerecord.sbt

import org.specs2.mutable._

class FieldSpec extends Specification {
  "Field" should {
    "apply" in {
      "name:string:required" in {
        Field(Seq("name", "string", "required")).toString mustEqual "@Required\n  name: String"
      }

      "age:int:option:range(min=0,max=100)" in {
        Field(Seq("age", "int", "option", "range(min=0,max=100)")).toString mustEqual "@Range(min=0,max=100)\n  age: Option[Int]"
      }

      "email:string:required:email" in {
        Field(Seq("email", "string", "required", "email")).toString mustEqual "@Required\n  @Email\n  email: String"
      }

      "ignore case" in {
        Field(Seq("name", "STRING", "REQUIRED")).toString mustEqual "@Required\n  name: String"
      }
    }

    "importStatement" in {
      "simple class name" in {
        Field("name", "String", false).importStatement must beNone
      }

      "qualified class name" in {
        Field("name", "com.github.aselab.SomeClass", false).importStatement must beSome("import com.github.aselab.SomeClass")
      }
    }

    "declaration" in {
      "isOption = true" in {
        Field("name", "com.github.aselab.SomeClass", true).declaration mustEqual "name: Option[SomeClass]"
      }

      "isOption = false" in {
        Field("name", "com.github.aselab.SomeClass", false).declaration mustEqual "name: SomeClass"
      }
    }
  }
}
