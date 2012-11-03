package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._

import dsl._
import java.util.{Date, UUID}
import java.sql.Timestamp
import models._

object ActiveRecordSpec extends ActiveRecordSpecification {
  override def before = {
    super.before
    TestTables.createTestData
  }

  "ActiveRecordCompanion" should {
    "#table returns corresponding table" >> {
      PrimitiveModel.table mustEqual TestTables.primitiveModels
    }

    "#all returns all records" >> {
      PrimitiveModel.all.toList must have size 100
    }

    "#apply calls find method" >> {
      PrimitiveModel.apply(3) mustEqual PrimitiveModel.find(3)
    }

    "#find searches by id and returns option result" >> {
      PrimitiveModel.find(13) must beSome.which {_.id == 13}
    }

    "#where" >> {
      "complex query" >> {
        PrimitiveModel.where {m: PrimitiveModel =>
          (m.int lte 30) and (m.string like "string%0")
        }.toList must have size 3
      }

      "null value" >> {
        PrimitiveModel.where {m: PrimitiveModel =>
          m.oint isNull
        }.toList must have size 50
      }
    }

    "#findBy(name, value)" >> {
      PrimitiveModel.findBy("string", "string33").map(_.string) must beSome("string33")
    }

    "#findBy(tuples)" >> {
      PrimitiveModel.findBy("string" -> "string33", "int" -> 33).map(_.string) must beSome("string33")
    }

    "#findAllBy" >> {
      "String" >> {
        val result = PrimitiveModel.findAllBy("string", "string33")
        result.toList must have size 1
        result.head.string mustEqual "string33"
      }

      "Option[String]" >> {
        val result = PrimitiveModel.findAllBy("ostring", "string33")
        result.toList must have size 1
        result.head.ostring must beSome("string33")
      }

      "Boolean" >> {
        val result = PrimitiveModel.findAllBy("boolean", true)
        result.toList must have size 50
      }

      "Option[Boolean]" >> {
        val result = PrimitiveModel.findAllBy("oboolean", true)
        result.toList must have size 25
      }

      "Int" >> {
        val result = PrimitiveModel.findAllBy("int", 55)
        result.toList must have size 1
        result.head.int mustEqual 55
      }

      "Option[Int]" >> {
        val result = PrimitiveModel.findAllBy("oint", 35)
        result.toList must have size 1
        result.head.oint must beSome(35)
      }

      "Long" >> {
        val result = PrimitiveModel.findAllBy("long", 55L)
        result.toList must have size 1
        result.head.long mustEqual 55L
      }

      "Option[Long]" >> {
        val result = PrimitiveModel.findAllBy("olong", 35L)
        result.toList must have size 1
        result.head.olong must beSome(35L)
      }

      "Float" >> {
        val result = PrimitiveModel.findAllBy("float", 23.toFloat)
        result.toList must have size 1
        result.head.float mustEqual 23.toFloat
      }

      "Option[Float]" >> {
        val result = PrimitiveModel.findAllBy("ofloat", 23.toFloat)
        result.toList must have size 1
        result.head.ofloat must beSome(23.toFloat)
      }

      "Double" >> {
        val result = PrimitiveModel.findAllBy("double", 45.0)
        result.toList must have size 1
        result.head.double mustEqual 45.0
      }

      "Option[Double]" >> {
        val result = PrimitiveModel.findAllBy("odouble", 45.0)
        result.toList must have size 1
        result.head.odouble must beSome(45.0)
      }

      "BigDecimal" >> {
        val result = PrimitiveModel.findAllBy("bigDecimal", BigDecimal(55))
        result.toList must have size 1
        result.head.bigDecimal mustEqual BigDecimal(55)
      }

      "Option[BigDecimal]" >> {
        val result = PrimitiveModel.findAllBy("obigDecimal", BigDecimal(45))
        result.toList must have size 1
        result.head.obigDecimal must beSome(BigDecimal(45))
      }

      "Timestamp" >> {
        val t = new Timestamp(44L)
        val result = PrimitiveModel.findAllBy("timestamp", t)
        result.toList must have size 1
        result.head.timestamp mustEqual t
      }

      "Option[Timestamp]" >> {
        val t = new Timestamp(44L)
        val result = PrimitiveModel.findAllBy("otimestamp", t)
        result.toList must have size 1
        result.head.otimestamp must beSome(t)
      }

      "Date" >> {
        val t = new Date(22L * 1000 * 60 * 60 * 24)
        val result = PrimitiveModel.findAllBy("date", t)
        result.toList must have size 1
        result.head.date.toString mustEqual "1970-01-23"
      }

      "Option[Date]" >> {
        val t = new Date(22L * 1000 * 60 * 60 * 24)
        val result = PrimitiveModel.findAllBy("odate", t)
        result.toList must have size 1
        result.head.odate must beSome.which {_.toString == "1970-01-23"}
      }

      "UUID" >> {
        val u = new UUID(11L, 11L)
        val result = PrimitiveModel.findAllBy("uuid", u)
        result.toList must have size 1
        result.head.uuid mustEqual u
      }

      "invalid field name" >> {
        PrimitiveModel.findAllBy("aaa", 1) must throwA(ActiveRecordException.notFoundField("aaa"))
      }

      "null, None" >> {
        PrimitiveModel.findAllBy("int", null) must throwA(ActiveRecordException.unsupportedType("int by null"))
        PrimitiveModel.findAllBy("oboolean", null).toList must have size 50
        PrimitiveModel.findAllBy("oboolean", None).toList must have size 50
      }

      "multiple values" >> {
        val result = PrimitiveModel.findAllBy("string" -> "string22", "int" -> 22)
        result.toList must have size 1
        result.head.int mustEqual 22
        PrimitiveModel.findAllBy("string" -> "string22", "int" -> 23) must beEmpty
      }
    }

    "RichQuery" >> {
      def query = RichQuery(PrimitiveModel.all)

      "#orderBy" >> {
        "single field" >> {
          query.orderBy(m => m.int desc).toList mustEqual PrimitiveModel.all.toList.reverse
        }

        "multiple fields" >> {
          query.orderBy(m => m.oint asc, m => m.int desc).toList mustEqual PrimitiveModel.all.toList.sortWith {
            (m1, m2) => (m1.oint, m2.oint) match {
              case (a, b) if a == b => m1.int > m2.int
              case (Some(a), Some(b)) => a < b
              case (None, Some(b)) => true
              case (Some(a), None) => false
              case _ => throw new Exception("")
            }
          }
        }
      }

      "#limit returns only specified count" >> {
        query.limit(10).toList mustEqual PrimitiveModel.all.toList.take(10)
      }

      "#page" >> {
        query.page(30, 10).toList mustEqual PrimitiveModel.all.toList.slice(30, 40)
      }

      "#count" >> {
        query.count mustEqual(100)
      }
    }

    "implicit conversions" >> {
      "query should be able to chain" >> {
        PrimitiveModel.all.where(m => m.int lt 50).findBy("string", "string22").map(_.string) must beSome("string22")
      }

      "ActiveRecordCompanion to RichQuery" >> {
        PrimitiveModel.limit(10).toList mustEqual PrimitiveModel.all.toList.take(10)
        PrimitiveModel.count mustEqual 100
      }

      "ActiveRecordCompanion to Queryable" >> {
        from(PrimitiveModel)(m => select(m)).toList mustEqual PrimitiveModel.all.toList
      }
    }

    "CRUD" >> {
      val m = PrimitiveModel.all.head
      PrimitiveModel.create(m)
      PrimitiveModel.find(m.id) must beSome(m)

      m.string = "test"
      m.int = 156
      PrimitiveModel.update(m)
      PrimitiveModel.find(m.id) must beSome(m)

      PrimitiveModel.delete(m.id)
      PrimitiveModel.find(m.id) must beNone
    }

    "#isUnique" >> {
      "returns true if value is null" >> {
        val m = PrimitiveModel.newInstance
        m.string = null
        PrimitiveModel.isUnique("string", m) must beTrue
      }

      "returns true if value is None" >> {
        val m = PrimitiveModel.newInstance
        m.ostring = None
        PrimitiveModel.isUnique("ostring", m) must beTrue
      }

      "on new record" >> {
        "returns false if value exists" >> {
          val m = PrimitiveModel.newInstance
          m.string = "string3"
          PrimitiveModel.isUnique("string", m) must beFalse
        }

        "returns true if value does not exists" >> {
          val m = PrimitiveModel.newInstance
          m.string = "aaabbb"
          PrimitiveModel.isUnique("string", m) must beTrue
        }
      }

      "on existing record" >> {
        trait testData extends Scope {
          val m = PrimitiveModel.find(3).get
        }

        "returns true if value does not modified" >> new testData {
          PrimitiveModel.isUnique("string", m) must beTrue
        }

        "returns false if value exists" >> new testData {
          m.string = "string55"
          PrimitiveModel.isUnique("string", m) must beFalse
        }

        "returns true if value does not exists" >> new testData {
          m.string = "aaabbb"
          PrimitiveModel.isUnique("string", m) must beTrue
        }
      }
    }
  }

  "ActiveRecord" should {
    "equals" >> {
      val m = PrimitiveModel.newModel(3)
      "with Product" >> {
        m.equals(PrimitiveModel.newModel(3)) must beTrue
      }

      "with AnyRef" >> {
        m.equals(new Object) must beFalse
      }

      "with null" >> {
        m.equals(null) must beFalse
      }
    }

    "#_companion returns companion object" >> {
      val m = PrimitiveModel.newModel(3)
      m._companion mustEqual PrimitiveModel
    }

    "CRUD" >> {
      val m = PrimitiveModel.newModel(5)
      val size = PrimitiveModel.all.size
      m.string = "abcdezzz"
      m.save
      PrimitiveModel.all.size mustEqual size + 1

      val oldId = m.id
      m.string = "aaaa"
      m.save
      m.id mustEqual oldId
      PrimitiveModel.find(oldId).get.string mustEqual "aaaa"

      m.delete
      PrimitiveModel.find(m.id) must beNone
    }

    "session" >> {
      "clean throws exception when not start session" >> {
        TestTables.clean must throwA[ActiveRecordException]
      }

      "start and clean" >> {
        TestTables.start
        val users: List[User] = User.all.toList
        User("testuser").save
        User("testuser2").save
        User("testuser3").save
        User.all.toList
        TestTables.clean
        User.all.toList mustEqual users
      }
    }

  }

  "database schema" should {
    def ddl = {
      var lines = List.empty[String]
      transaction { TestTables.printDdl(lines +:= _) }
      lines.mkString("\n")
    }

    "except @Transient column" >> {
      ddl must not matching("""(?ms).* transient_field .*""".r)
    }

    "be able to change column name by @Column" >> {
      ddl must not matching("""(?ms).* column_field .*""".r)
      ddl must matching("""(?ms).* columnName .*""".r)
    }

    "create unique index by @Unique" >> {
      ddl must matching("""(?ms).*create unique index \w+ on annotation_models \(unique_field\);.*""".r)
    }

    "except confirmation column" >> {
      ddl must not matching("""(?ms).* confirmation_field_confirmation .*""".r)
    }

    "except renamed confirmation column" >> {
      ddl must not matching("""(?ms).* confirmation_name .*""".r)
    }

  }

}
