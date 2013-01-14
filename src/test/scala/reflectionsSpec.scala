package com.github.aselab.activerecord

import org.specs2.mutable._

object reflectionsSpec extends Specification {
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

  class NotSupportedModel(func: (Int) => String)

  class ExceptionModel {
    throw new Exception
  }

  trait BaseModel {
    var string = "string"
    var int = 0
  }

  case class ExtendedModel(
    estring: String,
    eint: Int
  ) extends BaseModel

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
        val factory = ClassInfo(classOf[ComplexModel]).factory
        ClassInfo.factories.get(classOf[ComplexModel]).isDefined must beTrue
      }
    }

    "factory of NotSupportedModel" in {
      val c = classOf[NotSupportedModel]
      ClassInfo(c).factory must throwA(
        ActiveRecordException.cannotCreateInstance(c.getName, "No usable constructor is found. It is recommended to implement default constructor.")
      )
      success
    }

    "exception in constructor" in {
      val c = classOf[ExceptionModel]
      val factory = ClassInfo(c).factory
      factory.apply must throwA(
        ActiveRecordException.cannotCreateInstance(c.getName, null)
      )
    }
  }

  "ClassInfo#fieldInfo" should {
    "detect type of Option value" in {
      val c = new ClassInfo(classOf[models.PrimitiveModel])
      "Int" in {
        c.fieldInfo("oint") mustEqual
          FieldInfo("oint", classOf[Int], true, false)
      }

      "String" in {
         c.fieldInfo("ostring") mustEqual
           FieldInfo("ostring", classOf[String], true, false)
      }
    }

    "detect type of List value" in {
      val c = new ClassInfo(classOf[models.SeqModel])
      "Int" in {
        c.fieldInfo("list") mustEqual
          FieldInfo("list", classOf[Int], false, true)
      }

      "String" in {
         c.fieldInfo("seq") mustEqual
           FieldInfo("seq", classOf[Double], false, true)
      }
    }

    "includes superclass's fields" in {
      val c = new ClassInfo(classOf[ExtendedModel])
      c.fieldInfo.keys must contain("string", "int", "estring", "eint").only
    }
  }

  "ScalaSigInfo#genericTypes" should {
    "detect generic primitive types of Option field" in {
      ScalaSigInfo(classOf[models.PrimitiveModel]).genericTypes must contain(
        "ostring" -> classOf[String],
        "oboolean" -> classOf[Boolean],
        "oint" -> classOf[Int],
        "olong" -> classOf[Long],
        "ofloat" -> classOf[Float],
        "odouble" -> classOf[Double],
        "obigDecimal" -> classOf[BigDecimal],
        "otimestamp" -> classOf[java.sql.Timestamp],
        "odate" -> classOf[java.util.Date]
      )
    }

    "detect generic primitive types of Seq field" in {
      ScalaSigInfo(classOf[models.SeqModel]).genericTypes must contain(
        "list" -> classOf[Int],
        "seq" -> classOf[Double]
      )
    }
  }

  "Reflectable" should {
    import ReflectionUtil._
    "toOption" in {
      "Option[String]" in {
        val v = Some("a")
        v.toOption[String] mustEqual v
      }

      "Option[Int]" in {
        val v = Some(3)
        v.toOption[Int] mustEqual v
      }

      "None" in {
        None.toOption mustEqual None
      }

      "Int" in {
        3.toOption[Int] mustEqual Some(3)
      }

      "null" in {
        val v: String = null
        v.toOption mustEqual None
      }
    }
  }
}
