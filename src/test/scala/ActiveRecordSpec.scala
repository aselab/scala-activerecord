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
    DummyTables.createTestData
  }

  "ActiveRecordCompanion" should {
    "#table returns corresponding table" >> {
      DummyModel.table mustEqual DummyTables.dummyModels
    }

    "#all returns all records" >> {
      DummyModel.all must have size 100
    }

    "#apply calls find method" >> {
      DummyModel.apply(3) mustEqual DummyModel.find(3)
    }

    "#find searches by id and returns option result" >> {
      DummyModel.find(13) must beSome.which {_.id == 13}
    }

    "#where" >> {
      "complex query" >> {
        DummyModel.where {m: DummyModel =>
          (m.int lte 30) and (m.string like "string%0")
        } must have size 3
      }

      "null value" >> {
        DummyModel.where {m: DummyModel =>
          m.oint isNull
        } must have size 50
      }
    }

    "#findBy" >> {
      DummyModel.findBy("string", "string33").map(_.string) must beSome("string33")
    }

    "#findAllBy" >> {
      "String" >> {
        val result = DummyModel.findAllBy("string", "string33")
        result must have size 1
        result.head.string mustEqual "string33"
      }

      "Option[String]" >> {
        val result = DummyModel.findAllBy("ostring", Some("string33"))
        result must have size 1
        result.head.ostring must beSome("string33")
      }

      "Boolean" >> {
        val result = DummyModel.findAllBy("boolean", true)
        result must have size 50
      }

      "Option[Boolean]" >> {
        val result = DummyModel.findAllBy("oboolean", Some(true))
        result must have size 25
      }

      "Int" >> {
        val result = DummyModel.findAllBy("int", 55)
        result must have size 1
        result.head.int mustEqual 55
      }

      "Option[Int]" >> {
        val result = DummyModel.findAllBy("oint", Some(35))
        result must have size 1
        result.head.oint must beSome(35)
      }

      "Long" >> {
        val result = DummyModel.findAllBy("long", 55L)
        result must have size 1
        result.head.long mustEqual 55L
      }

      "Option[Long]" >> {
        val result = DummyModel.findAllBy("olong", Some(35L))
        result must have size 1
        result.head.olong must beSome(35L)
      }

      "Float" >> {
        val result = DummyModel.findAllBy("float", 23.toFloat)
        result must have size 1
        result.head.float mustEqual 23.toFloat
      }

      "Option[Float]" >> {
        val result = DummyModel.findAllBy("ofloat", Some(23.toFloat))
        result must have size 1
        result.head.ofloat must beSome(23.toFloat)
      }

      "Double" >> {
        val result = DummyModel.findAllBy("double", 45.0)
        result must have size 1
        result.head.double mustEqual 45.0
      }

      "Option[Double]" >> {
        val result = DummyModel.findAllBy("odouble", Some(45.0))
        result must have size 1
        result.head.odouble must beSome(45.0)
      }

      "BigDecimal" >> {
        val result = DummyModel.findAllBy("bigDecimal", BigDecimal(55))
        result must have size 1
        result.head.bigDecimal mustEqual BigDecimal(55)
      }

      "Option[BigDecimal]" >> {
        val result = DummyModel.findAllBy("obigDecimal", Some(BigDecimal(45)))
        result must have size 1
        result.head.obigDecimal must beSome(BigDecimal(45))
      }

      "Timestamp" >> {
        val t = new Timestamp(44L)
        val result = DummyModel.findAllBy("timestamp", t)
        result must have size 1
        result.head.timestamp mustEqual t
      }

      "Option[Timestamp]" >> {
        val t = Some(new Timestamp(44L))
        val result = DummyModel.findAllBy("otimestamp", t)
        result must have size 1
        result.head.otimestamp mustEqual t
      }

      "Date" >> {
        val t = new Date(22L * 1000 * 60 * 60 * 24)
        val result = DummyModel.findAllBy("date", t)
        result must have size 1
        result.head.date.toString mustEqual "1970-01-23"
      }

      "Option[Date]" >> {
        val t = Some(new Date(22L * 1000 * 60 * 60 * 24))
        val result = DummyModel.findAllBy("odate", t)
        result must have size 1
        result.head.odate must beSome.which {_.toString == "1970-01-23"}
      }

      "UUID" >> {
        val u = new UUID(11L, 11L)
        val result = DummyModel.findAllBy("uuid", u)
        result must have size 1
        result.head.uuid mustEqual u
      }

      "Option[UUID]" >> {
        val u = Some(new UUID(11L, 11L))
        val result = DummyModel.findAllBy("ouuid", u)
        result must have size 1
        result.head.ouuid mustEqual u
      }

      "multiple values" >> {
        val result = DummyModel.findAllBy("string" -> "string22", "int" -> 22)
        result must have size 1
        result.head.int mustEqual 22
        DummyModel.findAllBy("string" -> "string22", "int" -> 23) must beEmpty
      }
    }

    "RichQuery" >> {
      def query = RichQuery(DummyModel.all)

      "#orderBy" >> {
        "single field" >> {
          query.orderBy(m => m.int desc).toList mustEqual DummyModel.all.toList.reverse
        }

        "multiple fields" >> {
          query.orderBy(m => m.boolean asc, m => m.int desc).toList mustEqual DummyModel.all.toList.sortWith {
            (m1, m2) => m1.boolean < m2.boolean || m1.int > m2.int
          }
        }
      }

      "#limit returns only specified count" >> {
        query.limit(10).toList mustEqual DummyModel.all.toList.take(10)
      }

      "#page" >> {
        query.page(30, 10).toList mustEqual DummyModel.all.toList.slice(30, 40)
      }

      "#count" >> {
        query.count mustEqual(100)
      }
    }

    "implicit conversions" >> {
      "query should be able to chain" >> {
        DummyModel.all.where(m => m.int lt 50).findBy("string", "string22").map(_.string) must beSome("string22")
      }

      "Query to List" >> {
        val all: List[DummyModel] = DummyModel.all
        success
      }

      "ActiveRecordCompanion to RichQuery" >> {
        DummyModel.limit(10).toList mustEqual DummyModel.all.toList.take(10)
        DummyModel.count mustEqual 100
      }

      "ActiveRecordCompanion to Queryable" >> {
        from(DummyModel)(m => select(m)).toList mustEqual DummyModel.all.toList
      }
    }

    "CRUD" >> {
      val m = DummyModel.all.head
      DummyModel.create(m)
      DummyModel.find(m.id) must beSome(m)

      m.string = "test"
      m.int = 156
      DummyModel.update(m)
      DummyModel.find(m.id) must beSome(m)

      DummyModel.delete(m.id)
      DummyModel.find(m.id) must beNone
    }

    "#isUnique" >> {
      "returns true if value is null" >> {
        val m = DummyModel.newInstance
        m.string = null
        DummyModel.isUnique("string", m) must beTrue
      }

      "returns true if value is None" >> {
        val m = DummyModel.newInstance
        m.ostring = None
        DummyModel.isUnique("ostring", m) must beTrue
      }

      "on new record" >> {
        "returns false if value exists" >> {
          val m = DummyModel.newInstance
          m.string = "string3"
          DummyModel.isUnique("string", m) must beFalse
        }

        "returns true if value does not exists" >> {
          val m = DummyModel.newInstance
          m.string = "aaabbb"
          DummyModel.isUnique("string", m) must beTrue
        }
      }

      "on existing record" >> {
        trait testData extends Scope {
          val m = DummyModel.find(3).get
        }

        "returns true if value does not modified" >> new testData {
          DummyModel.isUnique("string", m) must beTrue
        }

        "returns false if value exists" >> new testData {
          m.string = "string55"
          DummyModel.isUnique("string", m) must beFalse
        }

        "returns true if value does not exists" >> new testData {
          m.string = "aaabbb"
          DummyModel.isUnique("string", m) must beTrue
        }
      }
    }
  }

  "ActiveRecord" should {
    "#_companion returns companion object" >> {
      val m = DummyModel.newModel(3)
      m._companion mustEqual DummyModel
    }

    "CRUD" >> {
      val m = DummyModel.newModel(5)
      val size = DummyModel.all.size
      m.string = "abcdezzz"
      m.save
      DummyModel.all.size mustEqual size + 1

      val oldId = m.id
      m.string = "aaaa"
      m.save
      m.id mustEqual oldId
      DummyModel.find(oldId).get.string mustEqual "aaaa"

      m.delete
      DummyModel.find(m.id) must beNone
    }

    "session" >> {
      "clean throws exception when not start session" >> {
        DummyTables.clean must throwA[ActiveRecordException]
      }

      "start and clean" >> {
        DummyTables.start
        val users: List[User] = User.all.toList
        User("testuser").save
        User("testuser2").save
        User("testuser3").save
        println(User.all.toList)
        DummyTables.clean
        User.all.toList mustEqual users
      }
    }

  }

}
