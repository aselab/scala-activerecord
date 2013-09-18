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
    lazy val profile = hasOne[Profile]
    lazy val address = hasOneThrough[Address, Profile](profile)
    lazy val optionAddress = hasOneThrough[Address, Profile](profile, foreignKey = "optionAddressId")
  }

  case class Group(name: String) extends ActiveRecord {
    lazy val user = hasOne[User]
    lazy val userByOtherKey = hasOne[User](foreignKey = "otherKey")
  }

  case class Profile(userId: Option[Long], optionAddressId: Option[Long]) extends ActiveRecord {
    val addressId: Long = optionAddressId.getOrElse(0)
    lazy val user = belongsTo[User]
    lazy val address = belongsTo[Address]
  }

  case class Address(country: String, city: String) extends ActiveRecord {
    lazy val profile = hasOne[Profile]
  }

  object User extends ActiveRecordCompanion[User]
  object Group extends ActiveRecordCompanion[Group]
  object Profile extends ActiveRecordCompanion[Profile]
  object Address extends ActiveRecordCompanion[Address]

  object Tables extends ActiveRecordTables {
    val users = table[User]
    val groups = table[Group]
    val profiles = table[Profile]
    val addresses = table[Address]
  }

  trait TestData extends Scope {
    val user = User("user1").create
    val group = Group("group1").create
    val address = Address("Japan", "Tokyo").create
    val profile = Profile(Some(user.id), Some(address.id)).create
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
      group.user := user
      val u: Option[User] = group.user
      u must beSome(user)
      group.user.name mustEqual user.name
    }
  }

  "HasOneThroughAssociation" should {
    "associate persisted record" >> new TestData {
      val newAddress = Address("aaa", "bbb").create
      user.address := newAddress
      Profile.exists(_.id === profile.id) must beFalse
      Address.exists(_.id === address.id) must beFalse
      user.address.toOption must beSome(newAddress)
    }

    "associate non-persisted record" >> new TestData {
      val newAddress = Address("aaa", "bbb")
      user.address.associate(newAddress) must throwA(ActiveRecordException.recordMustBeSaved)
    }

    "remove with not null constraint" >> new TestData {
      user.address.remove must throwA(ActiveRecordException.notNullConstraint("addressId"))
    }

    "remove" >> new TestData {
      val removed = user.optionAddress.remove
      removed must beSome(address)
      Profile.exists(_.id === profile.id) must beTrue
      Address.exists(_.id === address.id) must beTrue
      user.optionAddress.toOption must beNone
    }

    "delete with not null constraint" >> new TestData {
      val deleted = user.address.delete
      deleted must beSome(address)
      Profile.exists(_.id === profile.id) must beFalse
      Address.exists(_.id === address.id) must beFalse
      user.address.toOption must beNone
    }

    "delete" >> new TestData {
      val deleted = user.optionAddress.delete
      deleted must beSome(address)
      Profile.exists(_.id === profile.id) must beTrue
      Address.exists(_.id === address.id) must beFalse
      user.optionAddress.toOption must beNone
    }

    "implicit conversions" >> new TestData {
      val a: Option[Address] = user.address
      a must beSome(address)
      user.address.country mustEqual address.country
    }
  }
}
