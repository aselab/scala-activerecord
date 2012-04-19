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
    "table で対応するテーブルを取得できること" >> {
      DummyModel.table mustEqual DummyTables.dummyModels
    }

    "all で全件検索できること" >> {
      DummyModel.all must have size 100
    }

    "apply でfindを呼び出すこと" >> {
      DummyModel.apply(3) mustEqual DummyModel.find(3)
    }

    "find でid検索できること" >> {
      DummyModel.find(13) must beSome.which {_.id == 13}
    }

    "findBy" >> {
      "クエリ検索できること" >> {
        DummyModel.where {m: DummyModel =>
          (m.int lte 30) and (m.string like "string%0")
        } must have size 3
      }

      "クエリ検索でNoneを検索できること" >> {
        DummyModel.where {m: DummyModel =>
          m.oint isNull
        } must have size 50
      }

      "String値で検索できること" >> {
        val result = DummyModel.findBy("string", "string33")
        result must have size 1
        result.head.string mustEqual "string33"
      }

      "Option[String]値で検索できること" >> {
        val result = DummyModel.findBy("ostring", Some("string33"))
        result must have size 1
        result.head.ostring must beSome("string33")
      }

      "Boolean値で検索できること" >> {
        val result = DummyModel.findBy("boolean", true)
        result must have size 50
      }

      "Option[Boolean]値で検索できること" >> {
        val result = DummyModel.findBy("oboolean", Some(true))
        result must have size 25
      }

      "Int値で検索できること" >> {
        val result = DummyModel.findBy("int", 55)
        result must have size 1
        result.head.int mustEqual 55
      }

      "Option[Int]値で検索できること" >> {
        val result = DummyModel.findBy("oint", Some(35))
        result must have size 1
        result.head.oint must beSome(35)
      }

      "Long値で検索できること" >> {
        val result = DummyModel.findBy("long", 55L)
        result must have size 1
        result.head.long mustEqual 55L
      }

      "Option[Long]値で検索できること" >> {
        val result = DummyModel.findBy("olong", Some(35L))
        result must have size 1
        result.head.olong must beSome(35L)
      }

      "Float値で検索できること" >> {
        val result = DummyModel.findBy("float", 23.toFloat)
        result must have size 1
        result.head.float mustEqual 23.toFloat
      }

      "Option[Float]値で検索できること" >> {
        val result = DummyModel.findBy("ofloat", Some(23.toFloat))
        result must have size 1
        result.head.ofloat must beSome(23.toFloat)
      }

      "Double値で検索できること" >> {
        val result = DummyModel.findBy("double", 45.0)
        result must have size 1
        result.head.double mustEqual 45.0
      }

      "Option[Double]値で検索できること" >> {
        val result = DummyModel.findBy("odouble", Some(45.0))
        result must have size 1
        result.head.odouble must beSome(45.0)
      }

      "BigDecimal値で検索できること" >> {
        val result = DummyModel.findBy("bigDecimal", BigDecimal(55))
        result must have size 1
        result.head.bigDecimal mustEqual BigDecimal(55)
      }

      "Option[BigDecimal]値で検索できること" >> {
        val result = DummyModel.findBy("obigDecimal", Some(BigDecimal(45)))
        result must have size 1
        result.head.obigDecimal must beSome(BigDecimal(45))
      }

      "Timestamp値で検索できること" >> {
        val t = new Timestamp(44L)
        val result = DummyModel.findBy("timestamp", t)
        result must have size 1
        result.head.timestamp mustEqual t
      }

      "Option[Timestamp]値で検索できること" >> {
        val t = Some(new Timestamp(44L))
        val result = DummyModel.findBy("otimestamp", t)
        result must have size 1
        result.head.otimestamp mustEqual t
      }

      "Date値で検索できること" >> {
        val t = new Date(22L * 1000 * 60 * 60 * 24)
        val result = DummyModel.findBy("date", t)
        result must have size 1
        result.head.date.toString mustEqual "1970-01-23"
      }

      "Option[Date]値で検索できること" >> {
        val t = Some(new Date(22L * 1000 * 60 * 60 * 24))
        val result = DummyModel.findBy("odate", t)
        result must have size 1
        result.head.odate must beSome.which {_.toString == "1970-01-23"}
      }

      "UUID値で検索できること" >> {
        val u = new UUID(11L, 11L)
        val result = DummyModel.findBy("uuid", u)
        result must have size 1
        result.head.uuid mustEqual u
      }

      "Option[UUID]値で検索できること" >> {
        val u = Some(new UUID(11L, 11L))
        val result = DummyModel.findBy("ouuid", u)
        result must have size 1
        result.head.ouuid mustEqual u
      }

      "組み合わせで検索できること" >> {
        val result = DummyModel.findBy("string" -> "string22", "int" -> 22)
        result must have size 1
        result.head.int mustEqual 22
        DummyModel.findBy("string" -> "string22", "int" -> 23) must beEmpty
      }
    }

    "toRichQuery" >> {
      "findByをネストできること" >> {
        DummyModel.all.where(m => m.int lt 50).findBy("string", "string22") must have size 1
      }

      "orderByでqueryを並べ替えられること" >> {
        DummyModel.all.orderBy(m => m.int desc).toList mustEqual DummyModel.all.toList.reverse
      }

      "orderByで複数キーソートができること" >> {
        DummyModel.all.orderBy(m => m.boolean asc, m => m.int desc).toList mustEqual DummyModel.all.toList.sortWith {
          (m1, m2) => m1.boolean < m2.boolean || m1.int > m2.int
        }
      }

      "limitで指定件数取得できること" >> {
        DummyModel.all.limit(10).toList mustEqual DummyModel.all.toList.take(10)
      }
    }

    "CRUD できること" >> {
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

    "isUnique" >> {
      "値がnullの時trueを返すこと" >> {
        val m = DummyModel.newInstance
        m.string = null
        DummyModel.isUnique("string", m) must beTrue
      }

      "値がNoneの時trueを返すこと" >> {
        val m = DummyModel.newInstance
        m.ostring = None
        DummyModel.isUnique("ostring", m) must beTrue
      }

      "新規レコードの場合" >> {
        "重複する値のレコードがある場合falseを返すこと" >> {
          val m = DummyModel.newInstance
          m.string = "string3"
          DummyModel.isUnique("string", m) must beFalse
        }

        "重複する値のレコードがない場合trueを返すこと" >> {
          val m = DummyModel.newInstance
          m.string = "aaabbb"
          DummyModel.isUnique("string", m) must beTrue
        }
      }

      "既存レコードの場合" >> {
        trait testData extends Scope {
          val m = DummyModel.find(3).get
        }

        "値が変わっていない場合はtrueを返すこと" >> new testData {
          DummyModel.isUnique("string", m) must beTrue
        }

        "重複する値のレコードがある場合falseを返すこと" >> new testData {
          m.string = "string55"
          DummyModel.isUnique("string", m) must beFalse
        }

        "重複する値のレコードがない場合trueを返すこと" >> new testData {
          m.string = "aaabbb"
          DummyModel.isUnique("string", m) must beTrue
        }
      }
    }
  }

  "ActiveRecord" should {
    "companion でコンパニオンオブジェクトを取得できること" >> {
      val m = DummyModel.newModel(3)
      m._companion mustEqual DummyModel
    }

    "モデルのCRUDができること" >> {
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

    "toMap" >> {
      val m = DummyModel.newModel(5)
      m.ofloat = None
      m.otimestamp = None
      m.odate = None
      m.ouuid = None

      m.toMap must equalTo(Map(
        "boolean" -> true,
        "oboolean" -> true,
        "timestamp" -> new Timestamp(5L),
        "float" -> 5.0,
        "long" -> 5L,
        "olong" -> 5L,
        "string" -> "string5",
        "ostring" -> "string5",
        "bigDecimal" -> 5,
        "obigDecimal" -> 5,
        "double" -> 5.0,
        "odouble" -> 5.0,
        "date" -> new Date(5L * 1000 * 60 * 60 * 24),
        "int" -> 5,
        "oint" -> 5,
        "uuid" -> new UUID(5L, 5L)
      ))
    }

    "toMap (relation)" >> {
      val g = Group("group1")
      g.save
      val id = g.id
      g.users.associate(User("user1"))
      g.users.associate(User("user2"))
      g.toMap must equalTo(Map(
        "name" -> "group1",
        "users" -> List(
          Map("name" -> "user1", "groupId" -> id),
          Map("name" -> "user2", "groupId" -> id)
        )
      ))
      g.users.map(_.toMap) must equalTo(List(
        Map("name" -> "user1", "groupId" -> id, "group" -> Map("name" -> "group1")),
        Map("name" -> "user2", "groupId" -> id, "group" -> Map("name" -> "group1"))
      ))
    }
  }

}
