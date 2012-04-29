package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._
import models._
import dsl._

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
    
    "manyToMany relation" in {
      val p1 = Project("project1")
      val p2 = Project("project1")
      p1.save
      p2.save

      val p1users = User.all.toList
      p1users.foreach(p1.users.associate(_))
      val u4 = User("user4")
      val u5 = User("user5")
      u4.save
      u5.save
      User.all.foreach(_.projects.associate(p2))

      p1.users.toList must containAllOf(p1users)
      p1users.forall(u => u.projects must contain(p1, p2).only)
      u4.projects.toList must contain(p2).only
    }
  }

  "implicit conversions" should {
    "OneToMany relation to rich query" in {
      val g = Group.findBy("name", "group1").head
      g.users.findBy("name", "user2").toList mustEqual List(User("user2"))

      g.users.where(_.name like "user%").orderBy(_.name desc).toList mustEqual
        List(User("user2"), User("user1"))
    }

    "ManyToMany relation to rich query" in {
      val p = Project.findBy("name", "project1").head
      p.users.findBy("name", "user2").toList mustEqual List(User("user2"))

      p.users.where(_.name like "user%").orderBy(_.name desc).toList mustEqual
        List(User("user3"), User("user2"), User("user1"))
    }

    "OneToMany relation to List(model)" in {
      val group = Group.findBy("name", "group1").head
      val users: List[User] = group.users
      users mustEqual List(User("user1"), User("user2"))
    }

    "ManyToMany relation to List(model)" in {
      val project = Project.findBy("name", "project1").head
      val users: List[User] = project.users
      users must containAllOf(List(User("user1"), User("user2"), User("user3")))
    }

    "ManyToOne relation to Option(model)" in {
      val user = User.findBy("name", "user1").head
      val g: Option[Group] = user.group
      g must beSome(Group("group1"))
    }
  }
}
