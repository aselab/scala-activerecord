package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._
import models._

object RelationSpec extends ActiveRecordSpecification {
  "ActiveRecord" should {
    "oneToMany relation" in {
      val g = Group("group1")
      g.save

      val u1 = User("user1")
      val u2 = User("user2")
      g.users.associate(u1)
      g.users.associate(u2)
      User("user3").save

      g.users must contain(u1, u2).only
      u1.group.one must beSome(g)
      u2.group.one must beSome(g)
    }
  }
}
