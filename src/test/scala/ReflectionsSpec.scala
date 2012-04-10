package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._
import annotations._

object ReflectionSpec extends Specification {
  class Dummy {
    val s: String = "test"
  }

  "FieldInfo#apply" should {
    "Option値のタイプを検出できること" in {
      "Int" in {
        FieldInfo.apply("int", Some(3)) mustEqual
          FieldInfo("int", classOf[Integer], true, false)
      }

      "String" in {
         FieldInfo.apply("string", Some("test")) mustEqual
           FieldInfo("string", classOf[String], true, false)
      }

      "Object" in {
         FieldInfo.apply("object", Some(new Dummy)) mustEqual
           FieldInfo("object", classOf[Dummy], true, false)
      }
    }

    "List値のタイプを検出できること" in {
      "Int" in {
        FieldInfo.apply("int", Seq(3)) mustEqual
          FieldInfo("int", classOf[Integer], false, true)
      }

      "String" in {
         FieldInfo.apply("string", Seq("test")) mustEqual
           FieldInfo("string", classOf[String], false, true)
      }

      "Object" in {
         FieldInfo.apply("object", Seq(new Dummy)) mustEqual
           FieldInfo("object", classOf[Dummy], false, true)
      }
    }

    "Option[List]のタイプを検出できること" in {
       FieldInfo.apply("listint", Some(Seq(3, 4))) mustEqual
         FieldInfo("listint", classOf[Integer], true, true)

       FieldInfo.apply("listobject", Some(Seq(new Dummy))) mustEqual
         FieldInfo("listobject", classOf[Dummy], true, true)
    }
  }
}
