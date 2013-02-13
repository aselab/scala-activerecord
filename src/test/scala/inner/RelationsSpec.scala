package com.github.aselab.activerecord.inner

import org.specs2.mutable._

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import models._

object RelationsSpec extends ActiveRecordSpecification {
  override def before = {
    super.before
    TestTables.createTestData
  }

  "Relations" should {
    def relation = PrimitiveModel.all

    "#head" >> {
      relation.head mustEqual PrimitiveModel.all.toList.head
    }

    "#headOption" >> {
      relation.headOption mustEqual PrimitiveModel.all.toList.headOption
    }

    "#orderBy" >> {
      "single field" >> {
        relation.orderBy(m => m.int desc).toList mustEqual PrimitiveModel.all.toList.reverse
      }

      "multiple fields" >> {
        relation.orderBy(_.oint asc, _.int desc).toList mustEqual PrimitiveModel.all.toList.sortWith {
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
      relation.limit(10).toList mustEqual PrimitiveModel.all.toList.take(10)
    }

    "#page" >> {
      relation.page(30, 10).toList mustEqual PrimitiveModel.all.toList.slice(30, 40)
    }

    "#count" >> {
      relation.count mustEqual(100)
    }

    "#select" >> {
      relation.select(m => (m.id, m.string)).toList mustEqual PrimitiveModel.all.toList.map(m => (m.id, m.string))
    }

    "#joins" >> withRollback {
      val u1 = User("string50").create
      PrimitiveModel.joins[User]((p, u) => p.string === u.name)
        .where((p, u) => u.name === u1.name).headOption mustEqual
        PrimitiveModel.findBy("string", u1.name)
    }

    "toSql" >> {
      PrimitiveModel.all.toSql mustEqual
        inTransaction { PrimitiveModel.all.toQuery.statement }
      PrimitiveModel.where(m => m.id === 1).toSql mustEqual
        inTransaction { PrimitiveModel.where(m => m.id === 1).toQuery.statement }
    }

    "cache controls" >> {
      val user1 = User("user1").create
      val user2 = User("user2").create
      val users = User.where(_.name like "user%")
      users.toList mustEqual List(user1, user2)

      val user3 = User("user3").create
      users.toList mustEqual List(user1, user2)
      users.reload mustEqual List(user1, user2, user3)
    }

    "includes" >> {
      "eager loading (simple)" >> withRollback {
        val (user1, user2, user3) = (User("user1").create, User("user2").create, User("user3").create)
        val (group1, group2) = (Group("group1").create, Group("group2").create)
        group1.users << user1
        group1.users << user2
        group2.users << user3

        val List(g1, g2) = Group.where(_.name like "group%").includes(_.users).toList
        g1.users.isLoaded must beTrue
        g1.users.cache must contain(user1, user2).only
        g2.users.isLoaded must beTrue
        g2.users.cache must contain(user3).only
      }

      "eager loading (multiple associations)" >> withRollback {
        val (user1, user2, user3, user4) = (User("user1").create, User("user2", true).create, User("user3").create, User("user4").create)
        val (group1, group2) = (Group("group1").create, Group("group2").create)
        group1.users << user1
        group2.users << user3
        group2.adminUsers << user2
        group2.adminUsers << user4

        val List(g1, g2) = Group.where(_.name like "group%").includes(_.users, _.adminUsers).toList
        g1.users.isLoaded must beTrue
        g1.users.cache must contain(user1).only
        g2.users.isLoaded must beTrue
        g2.users.cache must contain(user2, user3, user4).only
        g2.adminUsers.isLoaded must beTrue
        g2.adminUsers.cache must contain(user2, user4).only
      }

      "eager loading (BelongsTo association)" >> withRollback {
        val (user1, user2, user3) = (User("user1").create, User("user2").create, User("user3").create)
        val (group1, group2) = (Group("group1").create, Group("group2").create)
        group1.users := List(user1, user2)
        group2.users << user3

        val users = group1.users.where(_.name like "user%").includes(_.group).toList
        users.map(_.group.isLoaded).forall(_ == true) must beTrue
        users.map(_.group.toOption).flatten must contain(group1, group1).only
      }
    }
  }
}
