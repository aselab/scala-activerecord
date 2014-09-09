package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import org.specs2.specification.Scope

package onetomany {
  case class User(name: String) extends ActiveRecord {
    val groupId: Long = 0
    val otherKey: Option[Long] = None

    lazy val group = belongsTo[Group]
    lazy val groupByOtherKey = belongsTo[Group](foreignKey = "otherKey")
  }

  case class Group(name: String) extends ActiveRecord {
    lazy val users = hasMany[User]
    lazy val usersByOtherKey = hasMany[User](foreignKey = "otherKey")
  }

  object User extends ActiveRecordCompanion[User]
  object Group extends ActiveRecordCompanion[Group]

  object Tables extends ActiveRecordTables {
    val users = table[User]
    val groups = table[Group]
  }

  trait TestData extends Scope {
    val user = User("user1").create
    val group = Group("group1").create
  }
}

object OneToManyAssociationSpec extends DatabaseSpecification {
  import onetomany._

  override def schema = Tables

  "BelongsToAssociation" should {
    "assign persisted record" >> new TestData {
      user.group := group
      user.group.isLoaded must beTrue
      user.group.cache mustEqual List(group)
      user.groupId mustEqual(group.id)
    }

    "assign non-persisted record" >> new TestData {
      val newGroup = Group("group2")
      user.group.assign(newGroup) must throwA(ActiveRecordException.recordMustBeSaved)
    }

    "associate persisted record" >> new TestData {
      user.group.associate(group)
      user.group.toOption must beSome(group)
    }

    "configure foreignKey" >> new TestData {
      user.groupByOtherKey.associate(group)
      user.groupByOtherKey.toOption must beSome(group)
      user.otherKey must beSome(group.id)
    }

    "implicit conversions" >> new TestData {
      user.group.associate(group)
      user.group.count mustEqual 1
      user.group.where(_.id === group.id).toList mustEqual List(group)
      user.group.name mustEqual group.name
    }
  }

  "HasManyAssociation" should {
    "assign to non-persisted record" >> new TestData {
      val newGroup = Group("group2")
      newGroup.users.assign(user) must throwA(ActiveRecordException.recordMustBeSaved)
    }

    "assign persisted record" >> new TestData {
      group.users.assign(user)
      user.groupId mustEqual(group.id)
    }

    "associate persisted record" >> new TestData {
      group.users.associate(user)
      group.users.toList mustEqual List(user)
    }

    "associate non-persisted record" >> new TestData {
      val newUser = User("user2")
      group.users.associate(newUser)
      newUser.isPersisted must beTrue
      group.users.toList mustEqual List(newUser)
    }

    "remove" >> new TestData {
      val user2 = User("user2").create
      group.usersByOtherKey := List(user, user2)
      val removed = group.usersByOtherKey.remove(user)
      removed must beSome(user)
      group.usersByOtherKey.toList mustEqual List(user2)
      group.usersByOtherKey.reload mustEqual List(user2)
    }

    "remove with not null constraint" >> new TestData {
      group.users << user
      group.users.remove(user) must throwA(ActiveRecordException.notNullConstraint("groupId"))
    }

    "removeAll" >> new TestData {
      val user2 = User("user2").create
      group.usersByOtherKey << user
      group.usersByOtherKey += user2
      val removed = group.usersByOtherKey.removeAll
      removed mustEqual List(user, user2)
      removed.forall(m => m.otherKey == None && m.isPersisted) must beTrue
      group.usersByOtherKey must beEmpty
      group.usersByOtherKey.reload must beEmpty
    }

    "removeAll with not null constraint" >> new TestData {
      group.users << user
      group.users.removeAll must throwA(ActiveRecordException.notNullConstraint("groupId"))
    }

    "deleteAll" >> new TestData {
      val user2 = User("user2").create
      val user3 = User("user3").create
      group.users << Seq(user, user2)
      group.users.deleteAll mustEqual List(user, user2)
      group.users must beEmpty
      User.exists(_.id === user.id) must beFalse
      User.exists(_.id === user2.id) must beFalse
      User.exists(_.id === user3.id) must beTrue
    }

    "append records" >> new TestData {
      val user2 = User("user2").create
      val user3 = User("user3").create
      group.users << user
      group.users ++= Seq(user2, user3)
      group.users.toList must contain(exactly(user, user2, user3))
    }

    "replace records" >> new TestData {
      val user2 = User("user2").create
      val user3 = User("user3").create
      group.usersByOtherKey << user
      group.usersByOtherKey := List(user2, user3)
      User.exists(_.id === user.id) must beTrue
      group.usersByOtherKey.toList mustEqual List(user2, user3)
    }

    "replace records with not null constraint" >> new TestData {
      val user2 = User("user2").create
      val user3 = User("user3").create
      group.users << user
      group.users := List(user2, user3)
      User.exists(_.id === user.id) must beFalse
      group.users.toList mustEqual List(user2, user3)
    }

    "implicit conversions" >> new TestData {
      group.users.associate(user)
      group.users.map(_.name).toList mustEqual List(user.name)
      group.users.where(_.id === user.id).toList mustEqual List(user)
    }
  }

}
