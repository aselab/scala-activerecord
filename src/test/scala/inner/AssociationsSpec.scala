package com.github.aselab.activerecord.inner

import org.specs2.mutable._
import org.specs2.specification.Scope
import com.github.aselab.activerecord._
import models._
import dsl._
import ActiveRecord._

object AssociationSpec extends ActiveRecordSpecification {
  trait Data extends Scope {
    val user = User("user1").create
    val group = Group("group1").create
  }

  "BelongsToAssociation" should {
    trait assoc extends Data {
      val association = new BelongsToAssociation[User, Group](user, "groupId")
    }

    "assign persisted record" in new assoc {
      association.assign(group)
      association.get must beSome(group)
      user.groupId must beSome(group.id)
    }

    "implicit conversions" in new assoc {
      association.assign(group)
      association.count mustEqual 1
      association.headOption must beSome(group)
      association.where(_.name === "group1").toList mustEqual List(group)
    }
  }

  "HasManyAssociation" should {
    trait assoc extends Data {
      val association = new HasManyAssociation[Group, User](group, Map(), "groupId")
    }

    "associate persisted record" in new assoc {
      association.associate(user)
      association.toList mustEqual List(user)
      user.groupId must beSome(group.id)
    }

    "implicit conversions" in new assoc {
      association.associate(user)
      association.headOption must beSome(user)
      association.where(_.name === "user1").headOption must beSome(user)
    }
  }
}

