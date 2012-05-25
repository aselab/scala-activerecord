package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._
import dsl._

import java.util.{Date, UUID}
import java.sql.Timestamp

object ValidationSpec extends ActiveRecordSpecification {
  class SaveableImpl extends Saveable {
    var calledMethods = List[String]()
    def save = {
      calledMethods :+= "save"
      true
    }
  }

  case class Dummy(e: Seq[String]) extends SaveableImpl with Validatable {
    override def doValidate {
      e.foreach(errors.add(_))
      calledMethods :+= "doValidate"
    }

    override def beforeValidation() {
      calledMethods :+= "beforeValidation"
    }
  }

  case class Dummy2(@Unique s1: String, @Required s2: String) extends ActiveRecord {
    def this() = this(null, null)
  }

  object Dummy2 extends ActiveRecordCompanion[Dummy2]

  case class Dummy3(
    @Length(min=3, max=10) length: String = "aaaaa",
    @Range(max=5.3) maxValue: Double = 0,
    @Range(min=0) minValue: Long = 1,
    @Range(min = 5, max = 10) range: Int = 7,
    @Checked checked: Boolean = true,
    @Email email: String = "test@example.com"
  ) extends ActiveRecord {
    def this() = this(null)
  }

  object Dummy3 extends ActiveRecordCompanion[Dummy3]

  case class ValidateModel(
    @Email email: String = ""
  ) extends ProductModel with CRUDable with ValidationSupport {
    def doDelete(): Boolean = true
    def doUpdate(): Boolean = true
    def doCreate(): Boolean = true
    def this() = this("")
  }

  object ValidateModel extends ProductModelCompanion[ValidateModel]

  case class FormSupportModel(
    string: String,
    boolean: Boolean,
    int: Int,
    long: Long,
    float: Float,
    double: Double,
    bigDecimal: BigDecimal,
    timestamp: Timestamp,
    date: Date,
    uuid: UUID,
    ostring: Option[String],
    oboolean: Option[Boolean],
    oint: Option[Int],
    olong: Option[Long],
    ofloat: Option[Float],
    odouble: Option[Double],
    obigDecimal: Option[BigDecimal],
    otimestamp: Option[Timestamp],
    odate: Option[Date],
    ouuid: Option[UUID]
  ) extends ActiveRecord {
    def this() = this("", false, 0, 0, 0.toFloat, 0.0, BigDecimal(0),
      new Timestamp(0), new Date(0), new UUID(0, 0),
      Some(""), Some(false), Some(0), Some(0L), Some(0.toFloat), Some(0.0),
      Some(BigDecimal(0)), Some(new Timestamp(0)), Some(new Date(0)), Some(new UUID(0, 0))
    )
  }

  object FormSupportModel extends ActiveRecordCompanion[FormSupportModel] with FormSupport[FormSupportModel]

  "Validatable" should {
    "addError" in {
      val m = Dummy(Nil)
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
        val m = new Dummy(Nil)
        m.validate must beTrue
      }

      "save" in {
        val m = new Dummy(Nil)
        m.save must beTrue
        m.calledMethods must contain("beforeValidation", "doValidate", "save").only
      }
    }

    "validate failure" in {
      "validate" in {
        val m = new Dummy(Seq("error"))
        m.validate must beFalse
      }

      "save" in {
        val m = new Dummy(Seq("error"))
        m.save must beFalse
        m.calledMethods must contain("beforeValidation", "doValidate").only
      }

    }

    "Validator" in {
      val dummyValidator =  ValidatorFactory[annotations.Unique]{(_, value) => if (value.toString == "dummy") Seq(("dummy", Nil)) else Nil}
      val dummyValidator2 = ValidatorFactory[annotations.Required]{(_, value) => if (value.toString == "dummy2") Seq(("dummy2", Nil)) else Nil}
      dummyValidator.register
      dummyValidator2.register

      "get" in {
        ValidatorFactory.get(classOf[annotations.Unique]) must beSome(dummyValidator)
        ValidatorFactory.get(classOf[annotations.Required]) must beSome(dummyValidator2)
      }

      "doValidate" in {
        "add custom annotations" in {
          val c = classOf[Dummy2]
          val m1 = Dummy2("dummy", "")
          val m2 = Dummy2("", "dummy2")
          val m3 = Dummy2("dummy", "dummy2")
          m1.validate
          m2.validate
          m3.validate
          m1.errors must contain(ValidationError(c, "s1", "dummy"))
          m2.errors must contain(ValidationError(c, "s2", "dummy2"))
          m3.errors must contain(ValidationError(c, "s1", "dummy"), ValidationError(c, "s2", "dummy2"))
        }

        "@Length" in {
          val c = classOf[Dummy3]
          val m1 = Dummy3(length = "")
          val m2 = Dummy3(length = "a" * 5)
          val m3= Dummy3(length = "a" * 11)
          m1.validate
          m2.validate
          m3.validate
          m1.errors must contain(ValidationError(c, "length", "minLength", 3))
          m2.errors must beEmpty
          m3.errors must contain(ValidationError(c, "length", "maxLength", 10))
        }

        "@Range max" in {
          val c = classOf[Dummy3]
          val m1 = Dummy3(maxValue = 5)
          val m2 = Dummy3(maxValue = 4)
          val m3 = Dummy3(maxValue = 6)
          m1.validate
          m2.validate
          m3.validate
          m1.errors must beEmpty
          m2.errors must beEmpty
          m3.errors must contain(ValidationError(c, "maxValue", "maxValue", 5.3))
        }

       "@Range min" in {
          val c = classOf[Dummy3]
          val m1 = Dummy3(minValue = 0)
          val m2 = Dummy3(minValue = 1)
          val m3 = Dummy3(minValue = -1)
          m1.validate
          m2.validate
          m3.validate
          m1.errors must beEmpty
          m2.errors must beEmpty
          m3.errors must contain(ValidationError(c, "minValue", "minValue", 0))
        }

       "@Range" in {
          val c = classOf[Dummy3]
          val models = List(Dummy3(range = 4), Dummy3(range = 5), Dummy3(range = 6),
            Dummy3(range = 9), Dummy3(range = 10), Dummy3(range = 11))
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

        "@Checked" in {
          val c = classOf[Dummy3]
          val m1 = Dummy3(checked = true)
          val m2 = Dummy3(checked = false)
          m1.validate
          m2.validate
          m1.errors must beEmpty
          m2.errors must contain(ValidationError(c, "checked", "checked"))
        }

        "@Email" in {
          val c = classOf[Dummy3]
          val m1 = Dummy3(email = "test@example.com")
          val m2 = Dummy3(email = "aaa")
          m1.validate
          m2.validate
          m1.errors must beEmpty
          m2.errors must contain(ValidationError(c, "email", "invalid"))
        }
      }

      "unregister" in {
        dummyValidator.unregister
        ValidatorFactory.get(classOf[annotations.Required]) must beSome(dummyValidator2)
        ValidatorFactory.get(classOf[annotations.Unique]) must beNone
      }

      "not extends ActivevRecord" in {
        val v = ValidateModel("aaa")
        v.validate
        v.errors must not beEmpty
      }
    }

    "bind" >> {
      FormSupportModel.bind(Map("string" -> "string", "ostring" -> "", "int" -> "100")) mustEqual
        new FormSupportModel().copy(string = "string", ostring = Some(""), int = 100)
    }
  }
}

