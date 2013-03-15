package com.github.aselab.activerecord.inner

import org.specs2.mutable._

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import models._
import java.sql.Timestamp

object RelationsSpec extends DatabaseSpecification with AutoRollback {
  override def beforeAll = {
    super.beforeAll
    TestTables.createTestData
  }

  "Relations" should {
    def relation = PrimitiveModel.all

    "#head" >> {
      relation.head mustEqual PrimitiveModel.all.toList.head
    }

    "#head (Exception)" >> {
      User.all.head must throwA[RecordNotFoundException]
    }

    "#headOption" >> {
      relation.headOption mustEqual PrimitiveModel.all.toList.headOption
    }

    "#last" >> {
      relation.last mustEqual PrimitiveModel.all.toList.last
    }

    "#last (Exception)" >> {
      User.all.last must throwA[RecordNotFoundException]
    }

    "#lastOption" >> {
      relation.lastOption mustEqual PrimitiveModel.all.toList.lastOption
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

      "use ExpressionNode" >> {
        relation.orderBy(_.int).toList mustEqual PrimitiveModel.all.toList
      }
    }

    "#reverse" >> {
      "empty order" >> {
        relation.reverse.toList mustEqual PrimitiveModel.all.toList.reverse
      }

      "single field order" >> {
        relation.orderBy(m => m.int desc).reverse.toList mustEqual PrimitiveModel.all.toList
      }

      "use ExpressionNode order" >> {
        relation.orderBy(_.int).reverse.toList mustEqual PrimitiveModel.all.toList.reverse
      }

      "multiple fields" >> {
        relation.orderBy(_.oint asc, _.int desc).reverse.toList mustEqual PrimitiveModel.all.toList.sortWith {
          (m1, m2) => (m1.oint, m2.oint) match {
            case (a, b) if a == b => m1.int > m2.int
            case (Some(a), Some(b)) => a < b
            case (None, Some(b)) => true
            case (Some(a), None) => false
            case _ => throw new Exception("")
          }
        }.reverse
      }

      "multiple reverse" >> {
        relation.reverse.reverse.toList mustEqual PrimitiveModel.all.toList
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

    "#compute" >> {
      relation.compute(m => max(m.id)) must beSome(100)
      relation.compute(m => min(m.ofloat)) must beSome(1.0)
      relation.compute(m => countDistinct(m.id)) mustEqual 100
    }

    "#max" >> {
      relation.max(_.id) mustEqual 100
      relation.max(_.ofloat) must beSome(50.0)
      relation.max(_.string) mustEqual "string99"
    }

    "#min" >> {
      relation.min(_.id) mustEqual 1
      relation.min(_.ofloat) must beSome(1.0)
      relation.min(_.string) mustEqual "string1"
    }

    "#average" >> {
      relation.average(_.id) mustEqual 51.0
      relation.average(_.oint) mustEqual 25.0
      relation.average(_.float) mustEqual 50.5
    }

    "#sum" >> {
      relation.sum(_.id) mustEqual 5050
      relation.sum(_.oint) must beSome(1275)
      relation.sum(_.float) mustEqual 5050.0
    }

    "#select" >> {
      relation.select(m => (m.id, m.string)).toList mustEqual PrimitiveModel.all.toList.map(m => (m.id, m.string))
    }

    "#distinct" >> {
      PrimitiveModel.newModel(1).save
      PrimitiveModel.where(_.string === "string1").select(_.string).distinct.count mustEqual 1
    }

    "#joins" >> {
      "one table" >> {
        val foo = Foo("string50").create
        val model = PrimitiveModel.findBy("string", foo.name)
        PrimitiveModel.joins[Foo]((p, f) => p.string === f.name)
          .where((_, f) => f.name === foo.name).headOption mustEqual model
      }

      "two tables" >> {
        val foo = Foo("string50").create
        val bar = Bar("string50").create
        val model = PrimitiveModel.findBy("string", foo.name)
        PrimitiveModel.joins[Foo, Bar]((p, foo, bar) =>
          (p.string === foo.name, p.string === bar.name)
        ).where((_, f, _) => f.name === foo.name).headOption mustEqual model
      }

      "three tables" >> {
        val foo = Foo("string50").create
        val bar = Bar("string50").create
        val baz = Baz("string50").create
        val model = PrimitiveModel.findBy("string", foo.name)
        PrimitiveModel.joins[Foo, Bar, Baz]((p, foo, bar, baz) =>
          (p.string === foo.name, p.string === bar.name, p.string === baz.name)
        ).where((_, f, _, _) => f.name === foo.name).headOption mustEqual model
      }
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

    "includes (eager loading)" >> {
      "HasMany association" >> {
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

      "multiple associations" >> {
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

      "BelongsTo association" >> {
        val (user1, user2, user3) = (User("user1").create, User("user2").create, User("user3").create)
        val (group1, group2) = (Group("group1").create, Group("group2").create)
        group1.users := List(user1, user2)
        group2.users << user3

        val users = group1.users.where(_.name like "user%").includes(_.group).toList
        users.map(_.group.isLoaded).forall(_ == true) must beTrue
        users.map(_.group.toOption).flatten must contain(group1, group1).only
      }

      "HABTM association" >> {
        val (foo1, foo2) = (Foo("foo1").create, Foo("foo2").create)
        val (bar1, bar2, bar3) = (Bar("bar1").create, Bar("bar2").create, Bar("bar3").create)
        foo1.bars := List(bar1, bar2)
        foo2.bars << bar3

        val List(f1, f2) = Foo.where(_.name like "foo%").includes(_.bars).toList
        f1.bars.isLoaded must beTrue
        f2.bars.isLoaded must beTrue
        f1.bars.cache must contain(bar1, bar2).only
        f2.bars.cache must contain(bar3).only
      }

      "HasManyThrough association" >> {
        val (project1, project2) = (Project("p1").create, Project("p2").create)
        val (group1, group2) = (Group("group1").create, Group("group2").create)
        val managers1 = (1 to 2).map(i => User("user" + i).create).toList
        val developers1 = (3 to 6).map(i => User("user" + i).create).toList
        val managers2 = (7 to 8).map(i => User("user" + i).create).toList
        val developers2 = (9 to 10).map(i => User("user" + i).create).toList
        project1.managers := managers1
        project1.developers := developers1
        project2.managers := managers2
        project2.developers := developers2
        group1.users << managers1.head
        group2.users << developers1.head

        val List(p1, p2) = Project.where(_.name like "p%")
          .includes(_.users, _.managers, _.developers, _.groups).toList
        List(p1, p2).forall(p => p.users.isLoaded &&  p.managers.isLoaded &&
          p.developers.isLoaded && p.groups.isLoaded) must beTrue

        p1.users.cache mustEqual managers1 ++ developers1
        p2.users.cache mustEqual managers2 ++ developers2
        p1.managers.cache mustEqual managers1
        p2.managers.cache mustEqual managers2
        p1.developers.cache mustEqual developers1
        p2.developers.cache mustEqual developers2
        p1.groups.cache mustEqual List(group1, group2)
        p2.groups.cache must beEmpty
      }
    }
  }
}
