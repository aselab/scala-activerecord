package com.github.aselab.activerecord.io

import org.specs2.mock._
import com.github.aselab.activerecord._
import dsl._
import models._
import validations._
import java.util.{Date, UUID}
import java.sql.Timestamp

object IOSpec extends DatabaseSpecification with Mockito {

  case class ListModel(l1: List[String], l2: List[Int]) extends ActiveModel
  object ListModel extends ActiveModelCompanion[ListModel] {
    def create(addErrors: (String, String)*) = {
      val l = ListModel(Nil, Nil)
      addErrors.foreach{ case (k, v) => l.errors.add(k, v) }
      l
    }
  }

  case class NestModel(
    @Required int: Int,
    list: ListModel
  ) extends ActiveModel {
    def this() = this(1, ListModel(List("a", "b"), List(1, 2)))
  }
  object NestModel extends ActiveModelCompanion[NestModel] {
    def create(l: ListModel, addErrors: (String, String)*) = {
      val n = NestModel(0, l)
      addErrors.foreach{ case (k, v) => n.errors.add(k, v) }
      n
    }
  }

  case class ComplexModel(
    @Required int: Int,
    nest: NestModel,
    nestlist: List[NestModel]
  ) extends ActiveModel {
    def this() = this(1, new NestModel, List(new NestModel, new NestModel))
  }
  object ComplexModel extends ActiveModelCompanion[ComplexModel]

  "IO" should {
    "toFormValues" in {
      "primitive and options" in {
        val m = PrimitiveModel.newModel(5)
        m.toFormValues mustEqual Map(
          "boolean" -> "true",
          "oboolean" -> "true",
          "timestamp" -> "1970-01-01T00:00:00.005Z",
          "otimestamp" -> "1970-01-01T00:00:00.005Z",
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
          "date" -> "1970-01-06",
          "odate" -> "1970-01-06",
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
        "timestamp" -> new Timestamp(5L),
        "otimestamp" -> new Timestamp(5L),
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
        "date" -> new Date(5L * 1000 * 60 * 60 * 24),
        "odate" -> new Date(5L * 1000 * 60 * 60 * 24),
        "int" -> 5,
        "oint" -> 5,
        "uuid" -> new UUID(5L, 5L)
      ))
      m must equalTo(PrimitiveModel.newModel(5))
    }

    "assgin(list)" in {
      val m = ListModel.newInstance
      m.assign(Map(
        "l1" -> List("aa", "bb", "cc"),
        "l2" -> List(1, 2, 3)
      ))
      m must equalTo(ListModel(List("aa", "bb", "cc"), List(1, 2, 3)))
    }

    "assgin(None)" in {
      val m = PrimitiveModel.newInstance
      m.assign(Map(
        "oint" -> "",
        "ostring" -> None,
        "odate" -> None
      ))
      m.oint must beNone
      m.ostring must beNone
      m.odate must beNone
    }

    "assgin(Some)" in {
      val m = PrimitiveModel.newInstance
      m.assign(Map(
        "oint" -> Some(1),
        "ostring" -> Some("aa")
      ))
      m.oint must beSome(1)
      m.ostring must beSome("aa")
    }

    "assgin(blank)" in {
      val m = PrimitiveModel.newInstance
      m.assign(Map(
        "ostring" -> Some("")
      ))
      m.ostring must beSome("")
    }

    "assignFormValues" in {
      "primitive types" in {
        val m = PrimitiveModel.newInstance
        m.assignFormValues(Map(
          "boolean" -> "true",
          "oboolean" -> "true",
          "timestamp" -> "1970-01-01T09:00:00.005+09:00",
          "otimestamp" -> "1970-01-01T09:00:00.005+09:00",
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
          "date" -> "1970-01-06",
          "odate" -> "1970-01-06",
          "int" -> "5",
          "oint" -> "5",
          "uuid" -> "00000000-0000-0005-0000-000000000005"
        ))
        m must equalTo(PrimitiveModel.newModel(5))
      }

      "option type" in {
        val m = PrimitiveModel.newInstance
        m.ostring = Some("aaa")
        m.oint = Some(34)
        m.olong = Some(33L)
        m.assignFormValues(Map(
          "oint" -> "",
          "ostring" -> ""
        ))
        m.ostring must beNone
        m.oint must beNone
        m.olong must beSome(33L)
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

      "complex model" in {
        val m = new ComplexModel
        m.assignFormValues(Map(
          "int" -> "3",
          "nest[int]" -> "2",
          "nest[list][l1][0]" -> "c",
          "nest[list][l1][1]" -> "d",
          "nest[list][l2][0]" -> "3",
          "nest[list][l2][1]" -> "4",
          "nestlist[0][int]" -> "2",
          "nestlist[0][list][l1][0]" -> "c",
          "nestlist[0][list][l1][1]" -> "d",
          "nestlist[0][list][l2][0]" -> "3",
          "nestlist[0][list][l2][1]" -> "4",
          "nestlist[1][int]" -> "2",
          "nestlist[1][list][l1][0]" -> "c",
          "nestlist[1][list][l1][1]" -> "d",
          "nestlist[1][list][l2][0]" -> "3",
          "nestlist[1][list][l2][1]" -> "4"
        ))
        val l1 = List("c", "d")
        val l2 = List(3, 4)
        val nest = NestModel(2, ListModel(l1, l2))
        m must equalTo(ComplexModel(3, nest, List(nest, nest)))
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
        m.errors must contain(exactly(ValidationError(classOf[ListModel], "l2", "activerecord.errors.invalid")))
      }
    }

    "toMap" in {
      val m = PrimitiveModel.newModel(5)
      m.ofloat = None
      m.otimestamp = None
      m.odate = None

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

    "formErrors" in {
      val listModel = ListModel.create(("", "global error1"), ("a", "field error1"), ("b", "field error2"))
      val nestModel = NestModel.create(listModel, ("", "global error2"), ("c", "field error3"))
      val listModelClass = listModel.getClass
      val nestModelClass = nestModel.getClass
      nestModel.formErrors must contain(exactly(
        ValidationError(nestModelClass, "list", "global error1"),
        ValidationError(listModelClass, "list[a]", "field error1"),
        ValidationError(listModelClass, "list[b]", "field error2"),
        ValidationError(nestModelClass, "", "global error2"),
        ValidationError(nestModelClass, "c", "field error3")
      ))
    }

    "formErrors(ComplexModel)" in {
      val listModel1 = ListModel.create(("", "global error1"), ("a", "field error1"), ("b", "field error2"))
      val listModel2 = ListModel.create(("", "global error2"), ("c", "field error3"), ("d", "field error4"))
      val listModel3 = ListModel.create(("", "global error3"), ("e", "field error5"), ("f", "field error6"))
      val nestModel1 = NestModel.create(listModel1, ("", "global error4"), ("g", "field error7"))
      val nestModel2 = NestModel.create(listModel2, ("", "global error5"), ("h", "field error8"))
      val nestModel3 = NestModel.create(listModel3, ("", "global error6"), ("i", "field error9"))
      val complexModel  = ComplexModel(1, nestModel1, List(nestModel2, nestModel3))
      complexModel.errors.add("global error7")
      complexModel.errors.add("j", "field error10")
      val listModelClass = listModel1.getClass
      val nestModelClass = nestModel1.getClass
      val complexModelClass = complexModel.getClass
      complexModel.formErrors must contain(exactly(
        ValidationError(nestModelClass, "nest[list]", "global error1"),
        ValidationError(listModelClass, "nest[list][a]", "field error1"),
        ValidationError(listModelClass, "nest[list][b]", "field error2"),
        ValidationError(nestModelClass, "nestlist[0][list]", "global error2"),
        ValidationError(listModelClass, "nestlist[0][list][c]", "field error3"),
        ValidationError(listModelClass, "nestlist[0][list][d]", "field error4"),
        ValidationError(nestModelClass, "nestlist[1][list]", "global error3"),
        ValidationError(listModelClass, "nestlist[1][list][e]", "field error5"),
        ValidationError(listModelClass, "nestlist[1][list][f]", "field error6"),
        ValidationError(complexModelClass, "nest", "global error4"),
        ValidationError(nestModelClass, "nest[g]", "field error7"),
        ValidationError(complexModelClass, "nestlist", "global error5"),
        ValidationError(nestModelClass, "nestlist[0][h]", "field error8"),
        ValidationError(complexModelClass, "nestlist", "global error6"),
        ValidationError(nestModelClass, "nestlist[1][i]", "field error9"),
        ValidationError(complexModelClass, "", "global error7"),
        ValidationError(complexModelClass, "j", "field error10")
      ))
    }

    "validate success" in {
      "validate" in {
        val m = new ComplexModel
        m.validate must beTrue
      }
    }

    "validate failure" in {
      "errors in basemodel" in {
        val m = new ComplexModel
        m.errors.add("error")
        m.validate must beFalse
      }

      "errors in nestmodel" in {
        val listModel1 = ListModel.create()
        val listModel2 = ListModel.create()
        val listModel3 = ListModel.create()
        val nestModel1 = NestModel.create(listModel1, ("", "error"))
        val nestModel2 = NestModel.create(listModel2)
        val nestModel3 = NestModel.create(listModel3)
        val m = ComplexModel(1, nestModel1, List(nestModel2, nestModel3))
        m.validate must beFalse
      }
    }
  }

  "FormUtil" should {
    "shift" in {
      FormUtil.shift("a[b][c]") mustEqual "b[c]"
      FormUtil.shift("a[b][c][d]") mustEqual "b[c][d]"
      FormUtil.shift("a[b]") mustEqual "b"
      FormUtil.shift("a") mustEqual "a"
    }

    "split" in {
      FormUtil.split("a[b]") mustEqual Seq("a", "b")
      FormUtil.split("a[b][c][d]") mustEqual Seq("a", "b", "c", "d")
      FormUtil.split("a[]") mustEqual Seq("a")
      FormUtil.split("a[][b]") mustEqual Seq("a", "", "b")
    }

    "join" in {
      FormUtil.join("a", "b") mustEqual "a[b]"
      FormUtil.join("a", "b", "c") mustEqual "a[b][c]"
      FormUtil.join("a", "b", "c", "d") mustEqual "a[b][c][d]"
      FormUtil.join("a", "b", "c[d]") mustEqual "a[b][c][d]"
    }
  }

  "FormSupport" should {
    "bind" in {
      "with no source" in {
        val m = PrimitiveModel.bind(Map("string" -> "string", "ostring" -> "", "int" -> "100"))
        m mustEqual PrimitiveModel.newInstance.copy(string = "string",
          int = 100, date = m.date, timestamp = m.timestamp, uuid = m.uuid)
      }

      "with source" in {
        val source = mock[PrimitiveModel]
        val data = Map("string" -> "string", "ostring" -> "", "int" -> "100")

        PrimitiveModel.bind(data)(source) mustEqual source

        there was one(source).assignFormValues(data)
      }
    }

    "unbind" in {
      val m = mock[PrimitiveModel]
      val data = Map("aaa" -> "aaa", "bbb" -> "bbb")
      m.toFormValues returns data
      PrimitiveModel.unbind(m) mustEqual data
    }

    "isRequired" in {
      ComplexModel.isRequired("int") must beTrue
      ComplexModel.isRequired("nest[int]") must beTrue
      ComplexModel.isRequired("nestlist[0][int]") must beTrue
      ComplexModel.isRequired("nest[list]") must beFalse
      ComplexModel.isRequired("nestlist[0][list]") must beFalse
      ComplexModel.isRequired("foo") must beFalse
      ComplexModel.isRequired("nestlist[][int]") must beTrue
    }
  }
}
