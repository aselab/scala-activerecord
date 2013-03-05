package com.github.aselab.activerecord.io

import org.specs2.mock._
import com.github.aselab.activerecord._
import dsl._
import models._
import validations._
import java.util.{Date, UUID}
import java.sql.Timestamp
import org.joda.time.{LocalDate, DateTime}

object IOSpec extends DatabaseSpecification with Mockito {

  case class ListModel(l1: List[String], l2: List[Int]) extends ActiveRecord
  object ListModel extends ActiveRecordCompanion[ListModel]

  case class NestModel(
    int: Int,
    list: ListModel
  ) extends ActiveRecord {
    def this() = this(1, ListModel(List("a", "b"), List(1, 2)))
  }
  object NestModel extends ActiveRecordCompanion[NestModel]

  case class ComplexModel(
    int: Int,
    nest: NestModel,
    nestlist: List[NestModel]
  ) extends ActiveRecord {
    def this() = this(1, new NestModel, List(new NestModel, new NestModel))
  }
  object ComplexModel extends ActiveRecordCompanion[ComplexModel]

  "IO" should {
    "toFormValues" in {
      "primitive and options" in {
        val m = PrimitiveModel.newModel(5)
        m.toFormValues mustEqual Map(
          "boolean" -> "true",
          "oboolean" -> "true",
          "float" -> "5.0",
          "ofloat" -> "5.0",
          "long" -> "5",
          "olong" -> "5",
          "string" -> "string5",
          "ostring" -> "string5",
          "bigDecimal" -> "5",
          "obigDecimal" -> "5",
          "double" -> "5.0",
          "odouble" -> "5.0",
          "int" -> "5",
          "oint" -> "5",
          "uuid" -> "00000000-0000-0005-0000-000000000005"
        )
      }

      "primitive list" in {
        val m = ListModel(List("aa", "bb", "cc"), List(11, 22, 33))
        m.toFormValues mustEqual Map(
          "l1[0]" -> "aa",
          "l1[1]" -> "bb",
          "l1[2]" -> "cc",
          "l2[0]" -> "11",
          "l2[1]" -> "22",
          "l2[2]" -> "33"
        )
      }

      "nest model" in {
        val m = new NestModel
        m.toFormValues mustEqual Map(
          "int" -> "1",
          "list[l1][0]" -> "a",
          "list[l1][1]" -> "b",
          "list[l2][0]" -> "1",
          "list[l2][1]" -> "2"
        )
      }

      "complex model" in {
        val m = new ComplexModel
        m.toFormValues mustEqual Map(
          "int" -> "1",
          "nest[int]" -> "1",
          "nest[list][l1][0]" -> "a",
          "nest[list][l1][1]" -> "b",
          "nest[list][l2][0]" -> "1",
          "nest[list][l2][1]" -> "2",
          "nestlist[0][int]" -> "1",
          "nestlist[0][list][l1][0]" -> "a",
          "nestlist[0][list][l1][1]" -> "b",
          "nestlist[0][list][l2][0]" -> "1",
          "nestlist[0][list][l2][1]" -> "2",
          "nestlist[1][int]" -> "1",
          "nestlist[1][list][l1][0]" -> "a",
          "nestlist[1][list][l1][1]" -> "b",
          "nestlist[1][list][l2][0]" -> "1",
          "nestlist[1][list][l2][1]" -> "2"
        )
      }
    }

    "assgin" in {
      val m = PrimitiveModel.newInstance
      m.assign(Map(
        "boolean" -> true,
        "oboolean" -> true,
        "float" -> 5.toFloat,
        "ofloat" -> 5.toFloat,
        "long" -> 5L,
        "olong" -> 5L,
        "string" -> "string5",
        "ostring" -> "string5",
        "bigDecimal" -> BigDecimal(5),
        "obigDecimal" -> BigDecimal(5),
        "double" -> 5.0,
        "odouble" -> 5.0,
        "int" -> 5,
        "oint" -> 5,
        "uuid" -> new UUID(5L, 5L)
      ))
      m must equalTo(PrimitiveModel.newModel(5))
    }

    "assignFormValues" in {
      "primitive types" in {
        val m = PrimitiveModel.newInstance
        m.assignFormValues(Map(
          "boolean" -> "true",
          "oboolean" -> "true",
          "float" -> "5.0",
          "ofloat" -> "5.0",
          "long" -> "5",
          "olong" -> "5",
          "string" -> "string5",
          "ostring" -> "string5",
          "bigDecimal" -> "5",
          "obigDecimal" -> "5",
          "double" -> "5.0",
          "odouble" -> "5.0",
          "int" -> "5",
          "oint" -> "5",
          "uuid" -> "00000000-0000-0005-0000-000000000005"
        ))
        m must equalTo(PrimitiveModel.newModel(5))
      }

      "list types" in {
        val m = ListModel.newInstance
        m.assignFormValues(Map(
          "l1[0]" -> "aa",
          "l1[1]" -> "bb",
          "l1[2]" -> "cc",
          "l2[0]" -> "11",
          "l2[1]" -> "22",
          "l2[2]" -> "33"
        ))
        m must equalTo(ListModel(List("aa", "bb", "cc"), List(11, 22, 33)))
      }

      "Validation error is added when form value is invalid" in {
        val m = ListModel.newInstance
        m.assignFormValues(Map(
          "l1[0]" -> "aaa",
          "l1[1]" -> "bbb",
          "l2[0]" -> "aaa",
          "l2[1]" -> "bbb"
        ))
        m must equalTo(ListModel(List("aaa", "bbb"), Nil))
        m.errors must contain(ValidationError(classOf[ListModel], "l2", "activerecord.errors.invalid")).only
      }
    }

    "toMap" in {
      val m = PrimitiveModel.newModel(5)
      m.ofloat = None

      m.toMap must equalTo(Map(
        "boolean" -> true,
        "oboolean" -> true,
        "float" -> 5.0,
        "long" -> 5L,
        "olong" -> 5L,
        "string" -> "string5",
        "ostring" -> "string5",
        "bigDecimal" -> 5,
        "obigDecimal" -> 5,
        "double" -> 5.0,
        "odouble" -> 5.0,
        "int" -> 5,
        "oint" -> 5,
        "uuid" -> new UUID(5L, 5L)
      ))
    }

    "persisted.toMap should include id" in {
      val m = PrimitiveModel.newModel(5).create
      m.toMap must contain("id" -> m.id)
    }
 
    "toMap (relation)" in {
      val g = Group("group1").create
      val p = Project("project1").create
      val id = g.id
      val u1 = User("user1")
      val u2 = User("user2")
      g.users.associate(u1)
      g.users.associate(u2)
      p.users.associate(u1)
      p.users.associate(u2)
      g.toMap must equalTo(Map(
        "name" -> "group1",
        "users" -> List(
          Map("name" -> "user1", "groupId" -> id),
          Map("name" -> "user2", "groupId" -> id)
        )
      ))
      g.users.map(_.toMap) must containAllOf(List(
        Map("name" -> "user1", "groupId" -> id, "group" -> Map("name" -> "group1"), "projects" -> List(Map("name" -> "project1"))),
        Map("name" -> "user2", "groupId" -> id, "group" -> Map("name" -> "group1"), "projects" -> List(Map("name" -> "project1")))
      ))

    }.pendingUntilFixed
  }

  "FormSupport" should {
    "bind" in {
      "with no source" in {
        PrimitiveModel.bind(Map("string" -> "string", "ostring" -> "", "int" -> "100")) mustEqual
          PrimitiveModel.newInstance.copy(string = "string", int = 100)
      }

      "with source" in {
        val source = mock[PrimitiveModel]
        val data = Map("string" -> "string", "ostring" -> "", "int" -> "100")

        PrimitiveModel.bind(data)(source) mustEqual source

        there was one(source).clearErrors andThen
          one(source).assignFormValues(data) andThen
          one(source).validate(false)
      }
    }

    "unbind" in {
      val m = mock[PrimitiveModel]
      val data = Map("aaa" -> "aaa", "bbb" -> "bbb")
      m.toFormValues returns data
      PrimitiveModel.unbind(m) mustEqual data
    }
  }
}
