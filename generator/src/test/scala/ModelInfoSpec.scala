package com.github.aselab.activerecord.generator

import org.specs2.mutable._

object ModelInfoSpec extends Specification {
  "ModelInfo" should {
    "apply" in {
      "name:string:required age:int" in {
        val fields = List(List("name", "string", "required"), List("age", "int"))
        ModelInfo(fields).map(_.toString) mustEqual Seq(
          "@Required name: String",
          "age: Int"
        )
      }

      "name:string:required age:int:option" in {
        val fields = List(List("name", "string", "required"), List("age", "int", "option"))
        ModelInfo(fields).map(_.toString) mustEqual Seq(
          "@Required name: String",
          "age: Option[Int]"
        )
      }

      "name:string:required:length(24) email:string:required:email age:int:option:range(min=0,max=150)" in {
        val fields = List(
          List("name", "string", "required", "length(24)"),
          List("email", "string", "required", "email"),
          List("age", "int", "option", "range(min=0,max=150)")
        )
        ModelInfo(fields).map(_.toString) mustEqual Seq(
          "@Required @Length(24) name: String",
          "@Required @Email email: String",
          "@Range(min=0,max=150) age: Option[Int]"
        )
      }

    }

    "getType" in {
      "string" in {
        ModelInfo.getType("string") mustEqual ("String", Nil)
      }

      "int" in {
        ModelInfo.getType("int") mustEqual ("Int", Nil)
      }

      "double" in {
        ModelInfo.getType("double") mustEqual ("Double", Nil)
      }

      "long" in {
        ModelInfo.getType("long") mustEqual ("Long", Nil)
      }

      "boolean" in {
        ModelInfo.getType("boolean") mustEqual ("Boolean", Nil)
      }

      "text" in {
        ModelInfo.getType("text") mustEqual ("String", Nil)
      }

      "Date" in {
        ModelInfo.getType("date") mustEqual ("Date", List("java.util.Date"))
      }
    }

    "getOption" in {
      "option" in {
        ModelInfo.getOption(List("option")) mustEqual (true, Nil)
      }

      "option:required" in {
        ModelInfo.getOption(List("option", "required")) mustEqual (true, List("Required"))
      }

      "required:email" in {
        ModelInfo.getOption(List("required", "email")) mustEqual (false, List("Required", "Email"))
      }

      "trim test" in {
        ModelInfo.getOption(List(" option ")) mustEqual (true, Nil)
      }

      "toLower test" in {
        ModelInfo.getOption(List("OPTION", "REQUIRED")) mustEqual (true, List("Required"))
      }
    }
  }
}
