package com.github.aselab.activerecord.io

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import models._
import org.json4s._

class JsonSupportSpec extends DatabaseSpecification {
  override def beforeAll() = {
    super.beforeAll()
    TestTables.createTestData
  }

  "JsonSerializer " should {
    "asJson" >> {
      User("foo", true).asJson must beEqualTo(JObject(List(JField("name",JString("foo")), JField("isAdmin",JBool(true)))))
    }

    "asJson(parameters)" >> {
      User("foo", true).asJson("name") must beEqualTo(JObject(List(JField("name",JString("foo")))))
      User("foo", true).asJson("name", "id", "isNewRecord") must beEqualTo(JObject(List(("name",JString("foo")), ("id",JInt(0)), ("isNewRecord",JBool(true)))))
    }

    "toJson" >> {
      User("foo", true).toJson must beEqualTo("""{"name":"foo","isAdmin":true}""")
    }

    "toJson(parameters)" >> {
      User("foo", true).toJson("name") must beEqualTo("""{"name":"foo"}""")
      User("foo", true).toJson("name", "id", "isNewRecord") must beEqualTo("""{"name":"foo","id":0,"isNewRecord":true}""")
    }

    "toJson(Date)" >> {
      DateModel.newModel(5).toJson("date") must beEqualTo("""{"date":"1970-01-06"}""")
    }

    "toJson(Timestamp)" >> {
      DateModel.newModel(5).toJson("timestamp") must beEqualTo("""{"timestamp":"1970-01-01T00:00:05.000Z"}""")
    }

    "toJson(UUID)" >> {
      PrimitiveModel.newModel(5).toJson("uuid") must beEqualTo("""{"uuid":"00000000-0000-0005-0000-000000000005"}""")
    }

    "toJson(Map)" >> {
      MapModel(Map("a" -> "b"), List(Map("c" -> "d"), Map("e" -> "f"))).toJson must beEqualTo("""{"mapData":{"a":"b"},"mapList":[{"c":"d"},{"e":"f"}]}""")
    }

    "fromJson" >> {
      User("hoge", false).fromJson("""{"name":"foo","isAdmin":true}""") must beEqualTo(User("foo", true))
    }

    "fromJson(Date)" >> {
      DateModel.newInstance.fromJson("""{"date":"1970-01-06"}""").date must beEqualTo(DateModel.newModel(5).date)
    }

    "fromJson(Timestamp)" >> {
      DateModel.newInstance.fromJson("""{"timestamp":"1970-01-01T00:00:05.00Z"}""").timestamp must beEqualTo(DateModel.newModel(5).timestamp)
    }

    "fromJson(UUID)" >> {
      PrimitiveModel.newInstance.fromJson("""{"uuid":"00000000-0000-0005-0000-000000000005"}""").uuid must beEqualTo(PrimitiveModel.newModel(5).uuid)
    }

    "fromJson(Map)" >> {
      MapModel.newInstance.fromJson("""{"mapData":{"a":"b"},"mapList":[{"c":"d"}{"e":"f"}]}""") must beEqualTo(MapModel(Map("a" -> "b"), List(Map("c" -> "d"), Map("e" -> "f"))))
    }
  }

  "JsonSupport" should {
    "fromJson" >> {
      User.fromJson("""{"name":"foo","isAdmin":true}""") must beEqualTo(User("foo", true))
    }

    "fromJson(PrimitiveModel)" >> {
      val json = PrimitiveModel.newModel(10).toJson
      json must beEqualTo("""
         |{"oboolean":false,"bigDecimal":"10","float":10.0,"ofloat":10.0,"uuid":"00000000-0000-000a-0000-00000000000a","olong":10,
         |"obigDecimal":"10","double":10.0,"long":10,"boolean":false,"int":10,"oint":10,"odouble":10.0,"string":"string10","ostring":"string10"}
      """.stripMargin.replaceAll("\n", "").trim)
      PrimitiveModel.fromJson(json) must beEqualTo(PrimitiveModel.newModel(10))
    }

    "fromJson(PrimitiveModel:persisted)" >> {
      val m = PrimitiveModel.newModel(10).create
      val json = m.toJson
      json must beEqualTo(s"""
         |{"oboolean":false,"bigDecimal":"10",
         |"float":10.0,"ofloat":10.0,"uuid":"00000000-0000-000a-0000-00000000000a","olong":10,"obigDecimal":"10",
         |"double":10.0,"long":10,"int":10,"oint":10,"odouble":10.0,"string":"string10","ostring":"string10","id":${m.id},"boolean":false}
      """.stripMargin.replaceAll("\n", "").trim)
      PrimitiveModel.fromJson(json) must beEqualTo(PrimitiveModel.newModel(10))
    }

    "fromArrayJson" >> {
      val json = """[{"name":"foo","isAdmin":true},{"name":"bar","isAdmin":false}]"""
      User.fromArrayJson(json) must beEqualTo(List(User("foo", true), User("bar", false)))
    }

    "fromJValue" >> {
      val jvalue = JObject(List(JField("name",JString("foo")), JField("isAdmin",JBool(true))))
      User.fromJValue(jvalue) must beEqualTo(User("foo", true))
    }

    "fromJArray" >> {
      val jarray = JArray(List(
        JObject(List(JField("name",JString("foo")), JField("isAdmin",JBool(true)))),
        JObject(List(JField("name",JString("bar")), JField("isAdmin",JBool(false))))
      ))

      User.fromJArray(jarray) must beEqualTo(List(User("foo", true), User("bar", false)))
    }

    "fromJson(list)" >> {
      val m = ListModel.newInstance
      val json = ListModel(List("aa", "bb", "cc"), List(1, 2, 3)).toJson
      ListModel.fromJson(json) must beEqualTo(ListModel(List("aa", "bb", "cc"), List(1, 2, 3)))
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
      json must beEqualTo("""{"int":1211,"nest":{"int":53,"list":{"l1":["vv","ss"],"l2":[33,44]}},"nestlist":[{"int":55,"list":{"l1":["f","e"],"l2":[4,5]}},{"int":111,"list":{"l1":["c","d"],"l2":[3,5]}}]}""")
      ComplexModel.newInstance.fromJson(json) must beEqualTo(m)
    }
  }

  "JsonImplicits" should {
    "asJson" >> {
      List(User("foo"), User("bar", true)).asJson must beEqualTo(JArray(List(
        JObject(List(JField("name",JString("foo")), JField("isAdmin",JBool(false)))),
        JObject(List(JField("name",JString("bar")), JField("isAdmin",JBool(true))))
      )))
    }

    "asJson(parameters)" >> {
      List(User("foo"), User("bar", true)).asJson("name", "isNewRecord") must beEqualTo(JArray(List(
        JObject(List(JField("name",JString("foo")), JField("isNewRecord",JBool(true)))),
        JObject(List(JField("name",JString("bar")), JField("isNewRecord",JBool(true))))
      )))
    }

    "toJson" >> {
      User("foo").save()
      User("bar", true).save()
      User.toList.toJson must beEqualTo("""[{"name":"foo","isAdmin":false,"id":1},{"name":"bar","isAdmin":true,"id":2}]""")
    }

    "toJson(parameters)" >> {
      PrimitiveModel.where(_.id.~ < 3).toList.toJson("string", "oint") must beEqualTo("""[{"string":"string1","oint":1},{"string":"string2","oint":2}]""")
    }

    "toJson(groupBy)" >> {
      List(User("foo"), User("bar", true), User("hoge", true)).groupBy(_.isAdmin).toJson must beEqualTo("""{"false":[{"name":"foo","isAdmin":false}],"true":[{"name":"bar","isAdmin":true},{"name":"hoge","isAdmin":true}]}""")
    }

    "toJson(groupBy, parameters)" >> {
      List(User("foo"), User("bar", true), User("hoge", true)).groupBy(_.isAdmin).toJson("name") must beEqualTo("""{"false":[{"name":"foo"}],"true":[{"name":"bar"},{"name":"hoge"}]}""")
    }
  }
}
