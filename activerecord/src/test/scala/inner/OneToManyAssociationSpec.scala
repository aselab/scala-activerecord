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

  override val config = Map(
    "schema" -> "com.github.aselab.activerecord.inner.onetomany.Tables"
  )

  "BelongsToAssociation" should {
    "assign persisted record" >> new TestData {
      user.group := group
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

    "deleteAll" >> new TestData {
      val user2 = User("user2").create
      val user3 = User("user3").create
      group.users := List(user, user2)
      group.users.deleteAll mustEqual List(user, user2)
      group.users must beEmpty
      User.exists(_.id === user.id) must beFalse
      User.exists(_.id === user2.id) must beFalse
      User.exists(_.id === user3.id) must beTrue
    }

    "implicit conversions" >> new TestData {
      group.users.associate(user)
      group.users.map(_.name).toList mustEqual List(user.name)
      group.users.where(_.id === user.id).toList mustEqual List(user)
    }
  }

}
