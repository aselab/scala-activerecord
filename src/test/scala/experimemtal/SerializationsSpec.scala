package com.github.aselab.activerecord.experimental

import org.specs2.mutable._
import org.specs2.specification._

import com.github.aselab.activerecord._
import models._
import java.util.{Date, UUID}
import java.sql.Timestamp

object SerializationsSpec extends ActiveRecordSpecification {
  "Serializable" should {
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
      val p = Project("project1")
      g.save
      p.save
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
    }
  }
}
