package com.github.aselab.activerecord

import org.specs2.mutable._

object supportSpec extends Specification {
  "allClasses" should {
    val f = support.allClasses.apply _

    "String" in {
      f("java.lang.String") == classOf[String] must beTrue
    }

    "Boolean" in {
      f("boolean") == classOf[Boolean] must beTrue
      f("java.lang.Boolean") == classOf[Boolean] must beTrue
      f("scala.Boolean") == classOf[Boolean] must beTrue
    }

    "Int" in {
      f("int") == classOf[Int] must beTrue
      f("java.lang.Integer") == classOf[Int] must beTrue
      f("scala.Int") == classOf[Int] must beTrue
    }

    "Long" in {
      f("long") == classOf[Long] must beTrue
      f("java.lang.Long") == classOf[Long] must beTrue
      f("scala.Long") == classOf[Long] must beTrue
    }

    "Float" in {
      f("float") == classOf[Float] must beTrue
      f("java.lang.Float") == classOf[Float] must beTrue
      f("scala.Float") == classOf[Float] must beTrue
    }

    "Double" in {
      f("double") == classOf[Double] must beTrue
      f("java.lang.Double") == classOf[Double] must beTrue
      f("scala.Double") == classOf[Double] must beTrue
    }

    "BigDecimal" in {
      f("scala.math.BigDecimal") == classOf[BigDecimal] must beTrue
    }

    "Timestamp" in {
      f("java.sql.Timestamp") == classOf[java.sql.Timestamp] must beTrue
    }

    "Date" in {
      f("java.util.Date") == classOf[java.util.Date] must beTrue
    }

    "UUID" in {
      f("java.util.UUID") == classOf[java.util.UUID] must beTrue
    }

    "User defined model class" in {
      f("com.github.aselab.activerecord.models.User") == classOf[models.User] must beTrue
    }

    "unsupported class" in {
      support.allClasses.isDefinedAt("com.github.aselab.activerecord.models.SeqModel") must beFalse
    }

  }
}
