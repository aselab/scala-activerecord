package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import org.specs2.specification.Scope

package onetoone {
  case class User(name: String) extends ActiveRecord {
    val groupId: Long = 0
    val otherKey: Option[Long] = None

    lazy val group = belongsTo[Group]
    lazy val groupByOtherKey = belongsTo[Group](foreignKey = "otherKey")
  }

  case class Group(name: String) extends ActiveRecord {
    lazy val user = hasOne[User]
    lazy val userByOtherKey = hasOne[User](foreignKey = "otherKey")
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

object OneToOneAssociationSpec extends DatabaseSpecification {
  import onetoone._

  override val config = Map(
    "schema" -> "com.github.aselab.activerecord.inner.onetoone.Tables"
  )

  "HasOneAssociation" should {
    "associate persisted record" >> new TestData {
      group.user.associate(user)
      group.user.toOption must beSome(user)
    }

    "associate non-persisted record" >> new TestData {
      val newUser = User("user2")
      group.user := newUser
      newUser.isPersisted must beTrue
      group.user.toOption must beSome(newUser)
    }

    "remove" >> new TestData {
      group.userByOtherKey := user
      val removed = group.userByOtherKey.remove
      removed must beSome(user)
      removed.forall(m => m.otherKey == None && m.isPersisted) must beTrue
      group.userByOtherKey.toOption must beNone
    }

    "remove with not null constraint" >> new TestData {
      group.user := user
      group.user.remove must throwA(ActiveRecordException.notNullConstraint("groupId"))
    }

    "delete" >> new TestData {
      group.user := user
      group.user.delete must beSome(user)
      group.user.toOption must beNone
      User.exists(_.id === user.id) must beFalse
    }

    "replace record" >> new TestData {
      val user2 = User("user2").create
      group.userByOtherKey := user
      group.userByOtherKey := user2
      User.exists(_.id === user.id) must beTrue
      group.userByOtherKey.toOption must beSome(user2)
    }

    "replace record with not null constraint" >> new TestData {
      val user2 = User("user2").create
      group.user := user
      group.user := user2
      User.exists(_.id === user.id) must beFalse
      group.user.toOption must beSome(user2)
    }

    "implicit conversions" >> new TestData {
      pending
    }
  }

}
