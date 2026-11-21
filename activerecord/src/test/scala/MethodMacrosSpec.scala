package com.github.aselab.activerecord

import org.specs2.mutable._
import scala.compiletime.testing.typeCheckErrors
import com.github.aselab.activerecord.dsl._
import models._

/** Compile-time checks of field names and value types done by MethodMacros */
class MethodMacrosSpec extends Specification {
  def messages(errors: List[scala.compiletime.testing.Error]): List[String] = errors.map(_.message)

  "newInstance" should {
    "accept known fields with matching types" >> {
      typeCheckErrors("""PrimitiveModel.newInstance("int" -> 1, "oint" -> 2, "ostring" -> None, "olong" -> Some(3L))""") must beEmpty
    }
    "reject an unknown field" >> {
      messages(typeCheckErrors("""PrimitiveModel.newInstance("nosuch" -> 1)""")) must contain(
        "value nosuch is not a member of com.github.aselab.activerecord.models.PrimitiveModel")
    }
    "reject a value of the wrong type" >> {
      messages(typeCheckErrors("""PrimitiveModel.newInstance("int" -> "x")""")).mkString must contain("type mismatch for int")
    }
  }

  "assign" should {
    "accept matching pairs and Map literals" >> {
      typeCheckErrors("""PrimitiveModel.newInstance.assign("string" -> "s", "oint" -> 3)""") must beEmpty
      typeCheckErrors("""PrimitiveModel.newInstance.assign(Map("string" -> "s", "long" -> 5L))""") must beEmpty
    }
    "reject a value of the wrong type" >> {
      messages(typeCheckErrors("""PrimitiveModel.newInstance.assign("string" -> 1)""")).mkString must contain("type mismatch for string")
    }
    "reject an unknown field in a Map literal" >> {
      messages(typeCheckErrors("""PrimitiveModel.newInstance.assign(Map("nosuch" -> 1))""")).mkString must contain("value nosuch is not a member")
    }
    "require a Map literal" >> {
      messages(typeCheckErrors("""{ val d = Map[String, Any]("int" -> 1); PrimitiveModel.newInstance.assign(d) }""")).mkString must contain("needs a Map(...) literal")
    }
  }

  "finders" should {
    "accept known fields with matching types" >> {
      typeCheckErrors("""PrimitiveModel.findBy("int" -> 1, "ostring" -> "a")""") must beEmpty
      typeCheckErrors("""PrimitiveModel.findAllBy("oboolean", null)""") must beEmpty
      typeCheckErrors("""PrimitiveModel.all.find(1L)""") must beEmpty
    }
    "reject an unknown field" >> {
      messages(typeCheckErrors("""PrimitiveModel.findAllBy("nosuch" -> 1)""")).mkString must contain("value nosuch is not a member")
      messages(typeCheckErrors("""PrimitiveModel.findBy("nosuch", 1)""")).mkString must contain("value nosuch is not a member")
    }
    "reject a value of the wrong type" >> {
      messages(typeCheckErrors("""PrimitiveModel.findBy("int", "x")""")).mkString must contain("type mismatch for int")
      messages(typeCheckErrors("""PrimitiveModel.all.find("x")""")).mkString must contain("type mismatch for id")
    }
    "require a literal field name" >> {
      messages(typeCheckErrors("""{ val k = "int"; PrimitiveModel.findBy(k -> 1) }""")).mkString must contain("must be a string literal")
    }
    "require at least one condition" >> {
      messages(typeCheckErrors("""PrimitiveModel.findBy()""")).mkString must contain("needs at least one argument")
    }
    "check the field names of findByOrCreate" >> {
      messages(typeCheckErrors("""PrimitiveModel.findByOrCreate(PrimitiveModel.newInstance, "nosuch")""")).mkString must contain("value nosuch is not a member")
    }
  }
}
