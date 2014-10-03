package com.github.aselab.activerecord.io

import com.github.aselab.activerecord._
import dsl._
import models._

object JsonSupportSpec extends DatabaseSpecification {
  "JsonSerializer " should {
    "toJson" >> {
      User("foo", true).toJson mustEqual """{"name":"foo","isAdmin":true}"""
    }

    "toJson(parameters)" >> {
      User("foo", true).toJson("name") mustEqual """{"name":"foo"}"""
      User("foo", true).toJson("name", "id", "isNewRecord") mustEqual """{"name":"foo","id":0,"isNewRecord":true}"""
    }

    "fromJson" >> {
      User("hoge", false).fromJson("""{"name":"foo","isAdmin":true}""") mustEqual User("foo", true)
    }
  }

  "JsonSupport" should {
    "fromJson" >> {
      User.fromJson("""{"name":"foo","isAdmin":true}""") mustEqual User("foo", true)
    }
  }

  "JsonImplicits" should {
    "toJson" >> {
      User("foo").save
      User("bar", true).save
      User.toList.toJson mustEqual """[{"name":"foo","isAdmin":false,"id":1},{"name":"bar","isAdmin":true,"id":2}]"""
    }

    "toJson(parameters)" >> {
      TestTables.createTestData
      PrimitiveModel.where(_.id.~ < 3).toList.toJson("string", "oint") mustEqual """[{"string":"string1","oint":1},{"string":"string2","oint":2}]"""
    }
  }
}
