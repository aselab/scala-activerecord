package com.github.aselab.activerecord.io

import com.github.aselab.activerecord._
import dsl._
import models._
import org.json4s._

object JsonSupportSpec extends DatabaseSpecification {
  override def beforeAll = {
    super.beforeAll
    TestTables.createTestData
  }

  "JsonSerializer " should {
    "asJson" >> {
      User("foo", true).asJson mustEqual JObject(List(JField("name",JString("foo")), JField("isAdmin",JBool(true))))
    }

    "asJson(parameters)" >> {
      User("foo", true).asJson("name") mustEqual JObject(List(JField("name",JString("foo"))))
      User("foo", true).asJson("name", "id", "isNewRecord") mustEqual JObject(List(("name",JString("foo")), ("id",JInt(0)), ("isNewRecord",JBool(true))))
    }

    "toJson" >> {
      User("foo", true).toJson mustEqual """{"name":"foo","isAdmin":true}"""
    }

    "toJson(parameters)" >> {
      User("foo", true).toJson("name") mustEqual """{"name":"foo"}"""
      User("foo", true).toJson("name", "id", "isNewRecord") mustEqual """{"name":"foo","id":0,"isNewRecord":true}"""
    }

    "toJson(Date)" >> {
      DateModel.newModel(5).toJson("date") mustEqual """{"date":"1970-01-06"}"""
    }

    "toJson(Timestamp)" >> {
      DateModel.newModel(5).toJson("timestamp") mustEqual """{"timestamp":"1970-01-01T00:00:05.000Z"}"""
    }

    "toJson(UUID)" >> {
      PrimitiveModel.newModel(5).toJson("uuid") mustEqual """{"uuid":"00000000-0000-0005-0000-000000000005"}"""
    }

    "toJson(Map)" >> {
      MapModel(Map("a" -> "b"), List(Map("c" -> "d"), Map("e" -> "f"))).toJson mustEqual """{"mapData":{"a":"b"},"mapList":[{"c":"d"},{"e":"f"}]}"""
    }

    "fromJson" >> {
      User("hoge", false).fromJson("""{"name":"foo","isAdmin":true}""") mustEqual User("foo", true)
    }

    "fromJson(Date)" >> {
      DateModel.newInstance.fromJson("""{"date":"1970-01-06"}""").date mustEqual DateModel.newModel(5).date
    }

    "fromJson(Timestamp)" >> {
      DateModel.newInstance.fromJson("""{"timestamp":"1970-01-01T00:00:05.00Z"}""").timestamp mustEqual DateModel.newModel(5).timestamp
    }

    "fromJson(UUID)" >> {
      PrimitiveModel.newInstance.fromJson("""{"uuid":"00000000-0000-0005-0000-000000000005"}""").uuid mustEqual PrimitiveModel.newModel(5).uuid
    }

    "fromJson(Map)" >> {
      MapModel.newInstance.fromJson("""{"mapData":{"a":"b"},"mapList":[{"c":"d"}{"e":"f"}]}""") mustEqual MapModel(Map("a" -> "b"), List(Map("c" -> "d"), Map("e" -> "f")))
    }
  }

  "JsonSupport" should {
    "fromJson" >> {
      User.fromJson("""{"name":"foo","isAdmin":true}""") mustEqual User("foo", true)
    }

    "fromJson(PrimitiveModel)" >> {
      val json = PrimitiveModel.newModel(10).toJson
      json mustEqual """
         |{"oboolean":false,"bigDecimal":"10",
         |"float":10.0,"ofloat":10.0,"uuid":"00000000-0000-000a-0000-00000000000a","olong":10,"string":"string10",
         |"ostring":"string10","obigDecimal":"10","double":10.0,"long":10,"boolean":false,"int":10,"oint":10,"odouble":10.0}
      """.stripMargin.replaceAll("\n", "").trim
      PrimitiveModel.fromJson(json) mustEqual PrimitiveModel.newModel(10)
    }

    "fromJson(PrimitiveModel:persisted)" >> {
      val m = PrimitiveModel.newModel(10).create
      val json = m.toJson
      json mustEqual s"""
         |{"oboolean":false,"bigDecimal":"10",
         |"float":10.0,"ofloat":10.0,"uuid":"00000000-0000-000a-0000-00000000000a","olong":10,"string":"string10",
         |"ostring":"string10","obigDecimal":"10","double":10.0,"long":10,"id":${m.id},"boolean":false,
         |"int":10,"oint":10,"odouble":10.0}
      """.stripMargin.replaceAll("\n", "").trim
      PrimitiveModel.fromJson(json) mustEqual PrimitiveModel.newModel(10)
    }

    "fromArrayJson" >> {
      val json = """[{"name":"foo","isAdmin":true},{"name":"bar","isAdmin":false}]"""
      User.fromArrayJson(json) mustEqual List(User("foo", true), User("bar", false))
    }

    "fromJValue" >> {
      val jvalue = JObject(List(JField("name",JString("foo")), JField("isAdmin",JBool(true))))
      User.fromJValue(jvalue) mustEqual User("foo", true)
    }

    "fromJArray" >> {
      val jarray = JArray(List(
        JObject(List(JField("name",JString("foo")), JField("isAdmin",JBool(true)))),
        JObject(List(JField("name",JString("bar")), JField("isAdmin",JBool(false))))
      ))

      User.fromJArray(jarray) mustEqual List(User("foo", true), User("bar", false))
    }

    "fromJson(list)" >> {
      val m = ListModel.newInstance
      val json = ListModel(List("aa", "bb", "cc"), List(1, 2, 3)).toJson
      ListModel.fromJson(json) mustEqual  ListModel(List("aa", "bb", "cc"), List(1, 2, 3))
    }

    "fromJson(nest)" >> {
      val m = ComplexModel(
        1211,
        NestModel(53,ListModel(List("vv", "ss"),List(33, 44))),
        List(
          NestModel(55, ListModel(List("f", "e"),List(4, 5))),
          NestModel(111, ListModel( List("c", "d"), List(3, 5)))
        )
      )
      val json = m.toJson
      json mustEqual """{"int":1211,"nest":{"int":53,"list":{"l1":["vv","ss"],"l2":[33,44]}},"nestlist":[{"int":55,"list":{"l1":["f","e"],"l2":[4,5]}},{"int":111,"list":{"l1":["c","d"],"l2":[3,5]}}]}"""
      ComplexModel.newInstance.fromJson(json) mustEqual m
    }
  }

  "JsonImplicits" should {
    "asJson" >> {
      List(User("foo"), User("bar", true)).asJson mustEqual JArray(List(
        JObject(List(JField("name",JString("foo")), JField("isAdmin",JBool(false)))),
        JObject(List(JField("name",JString("bar")), JField("isAdmin",JBool(true))))
      ))
    }

    "asJson(parameters)" >> {
      List(User("foo"), User("bar", true)).asJson("name", "isNewRecord") mustEqual JArray(List(
        JObject(List(JField("name",JString("foo")), JField("isNewRecord",JBool(true)))),
        JObject(List(JField("name",JString("bar")), JField("isNewRecord",JBool(true))))
      ))
    }

    "toJson" >> {
      User("foo").save
      User("bar", true).save
      User.toList.toJson mustEqual """[{"name":"foo","isAdmin":false,"id":1},{"name":"bar","isAdmin":true,"id":2}]"""
    }

    "toJson(parameters)" >> {
      PrimitiveModel.where(_.id.~ < 3).toList.toJson("string", "oint") mustEqual """[{"string":"string1","oint":1},{"string":"string2","oint":2}]"""
    }

    "toJson(groupBy)" >> {
      List(User("foo"), User("bar", true), User("hoge", true)).groupBy(_.isAdmin).toJson mustEqual
        """{"false":[{"name":"foo","isAdmin":false}],"true":[{"name":"bar","isAdmin":true},{"name":"hoge","isAdmin":true}]}"""
    }

    "toJson(groupBy, parameters)" >> {
      List(User("foo"), User("bar", true), User("hoge", true)).groupBy(_.isAdmin).toJson("name") mustEqual
        """{"false":[{"name":"foo"}],"true":[{"name":"bar"},{"name":"hoge"}]}"""
    }
  }
}
