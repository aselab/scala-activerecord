package com.github.aselab.activerecord

import org.specs2.mutable._
import models._
import dsl._

object RelationSpec extends ActiveRecordSpecification {
  "ActiveRecord" should {
    "oneToMany relation" in {
      val g = Group("group1").create

      val u1 = User("user1")
      val u2 = User("user2")
      g.users.associate(u1)
      g.users.associate(u2)
      User("user3").save

      g.users must contain(u1, u2).only
      u1.group.one must beSome(g)
      u2.group.one must beSome(g)
    }

    "manyToMany(hasManyThrough) relation" >> {
      val p1 = Project("project1").create
      val p2 = Project("project2").create
      val r1 = Role("role1").create
      val r2 = Role("role2").create

      val List(u1, u2, u3) = User.all.toList

      val membership = p1.users.assign(u1)
      r1.memberships.associate(membership)

      p1.users.associate(u2, ProjectMembership(r2.id))
      p2.users.associate(u2, ProjectMembership(r1.id))

      ProjectMembership.all.count mustEqual 3
      p1.users.toList must contain(u1, u2).only
      u2.projects.toList must contain(p1, p2).only 
      u2.memberships.toList mustEqual u2.projects.associations
      u2.memberships.map(m => (m.projectId, m.userId, m.roleId)) must
        contain((p1.id, u2.id, r2.id), (p2.id, u2.id, r1.id)).only
    }

    "manyToMany(hasAndBelongsToMany) relation" in {
      val foo1 = Foo("foo1").create
      val foo2 = Foo("foo2").create
      val bar1 = Bar("bar1").create
      val bar2 = Bar("bar2").create
      val bar3 = Bar("bar3").create

      foo1.bars.associate(bar2)
      foo1.bars.associate(bar3)
      bar1.foos.associate(foo2)
      bar3.foos.associate(foo2)

      foo1.bars.toList must contain(bar2, bar3).only
      foo2.bars.toList must contain(bar1, bar3).only
      bar3.foos.toList must contain(foo1, foo2).only
    }
  }

  "implicit conversions" should {
    "OneToMany relation to rich query" in {
      val g = Group.findBy("name", "group1").get
      g.users.findBy("name", "user2") must beSome(User("user2"))

      g.users.where(_.name like "user%").orderBy(_.name desc).toList mustEqual
        List(User("user2"), User("user1"))
    }

    /*
    "ManyToMany relation to rich query" in {
      val foo = Foo.findBy("name", "foo1").get
      foo.bars.findBy("name", "bar2") must beSome(Bar("bar2"))

      foo.bars.where(_.name like "bar%").orderBy(_.name desc).toList mustEqual
        List(Bar("bar3"), Bar("bar2"))
    }
    */

    "OneToMany relation to List(model)" in {
      val group = Group.findBy("name", "group1").get
      val users: List[User] = group.users
      success
    }

    "ManyToMany relation to List(model)" in {
      val project = Project.findBy("name", "project1").get
      val users: List[User] = project.users
      success
    }

    "ManyToOne relation to Option(model)" in {
      val user = User.findBy("name", "user1").get
      val g: Option[Group] = user.group
      success
    }
  }
}
