package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._
import annotations._

object ReflectionSpec extends Specification {
  class Dummy {
    val s: String = "test"
  }

  class PrimitiveModel(
    string: String,
    boolean: Boolean,
    int: Int,
    long: Long,
    float: Float,
    double: Double,
    date: java.util.Date,
    timestamp: java.sql.Timestamp,
    uuid: java.util.UUID,
    bigDecimal: BigDecimal
  )

  class MultipleConstructor(
    val string: String,
    boolean: Boolean,
    int: Int,
    long: Long,
    float: Float,
    double: Double
  ) extends PrimitiveModel(
    string, boolean, int, long, float, double, null, null, null, null
  ) {
    def this(string: String, boolean: Boolean, int: Int, long: Long) =
      this(string, boolean, int, long, 1.toFloat, 1.0)

    def this(string: String, boolean: Boolean) = this(string, boolean, 2, 2)

    def this(string: String) = this(string, true)

    def this() = this("test")
  }

  class ComplexModel(
    model: PrimitiveModel,
    optionInt: Option[Int],
    optionString: Option[String],
    optionModel: Option[PrimitiveModel],
    byteArray: Array[Byte],
    list: List[Int]
  )

  "ClassInfo" should {
    "factory of PrimitiveModel" in {
      val factory = ClassInfo(classOf[PrimitiveModel]).factory
      
      "can construct new instance" in {
        val m = factory.apply
        m.isInstanceOf[PrimitiveModel] must beTrue
        factory.apply must not equalTo(m)
      }

      "is added to ClassInfo.factories" in {
        ClassInfo.factories.get(classOf[PrimitiveModel]) must beSome(factory)
      }
    }

    "factory of MultipleConstructor" in {
      val factory = ClassInfo(classOf[MultipleConstructor]).factory

      "uses a constructor which has minimum arguments size" in {
        factory.apply.string mustEqual "test"
      }
    }

    "factory of ComplexModel" in {
      "can construct new instance" in {
        val factory = ClassInfo(classOf[ComplexModel]).factory
        val m = factory.apply
        m.isInstanceOf[ComplexModel] must beTrue
        factory.apply must not equalTo(m)
      }

      "is added to ClassInfo.factories" in {
        ClassInfo.factories - classOf[PrimitiveModel]
        val factory = ClassInfo(classOf[ComplexModel]).factory
        ClassInfo.factories.get(classOf[ComplexModel]).isDefined must beTrue
        ClassInfo.factories.get(classOf[PrimitiveModel]).isDefined must beTrue
      }
    }
  }

  "FieldInfo#apply" should {
    "detect type of Option value" in {
      "Int" in {
        FieldInfo.apply("int", Some(3)) mustEqual
          FieldInfo("int", Some(classOf[java.lang.Integer]), true, false)
      }

      "String" in {
         FieldInfo.apply("string", Some("test")) mustEqual
           FieldInfo("string", Some(classOf[String]), true, false)
      }

      "Object" in {
         FieldInfo.apply("object", Some(new Dummy)) mustEqual
           FieldInfo("object", Some(classOf[Dummy]), true, false)
      }
    }

    "detect type of List value" in {
      "Int" in {
        FieldInfo.apply("int", Seq(3)) mustEqual
          FieldInfo("int", Some(classOf[java.lang.Integer]), false, true)
      }

      "String" in {
         FieldInfo.apply("string", Seq("test")) mustEqual
           FieldInfo("string", Some(classOf[String]), false, true)
      }

      "Object" in {
         FieldInfo.apply("object", Seq(new Dummy)) mustEqual
           FieldInfo("object", Some(classOf[Dummy]), false, true)
      }
    }

    "Option[List]" in {
       FieldInfo.apply("listint", Some(Seq(3, 4))) mustEqual
         FieldInfo("listint", Some(classOf[java.lang.Integer]), true, true)

       FieldInfo.apply("listobject", Some(Seq(new Dummy))) mustEqual
         FieldInfo("listobject", Some(classOf[Dummy]), true, true)
    }
  }
}
