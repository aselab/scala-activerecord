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
      val association = new BelongsToAssociation[User, Group](user)
    }

    "assign persisted record" in new assoc {
      association.assign(group)
      association.get must beSome(group)
      user.groupId must beSome(group.id)
    }
  }

  "HasManyAssociation" should {
    trait assoc extends Data {
      val association = new HasManyAssociation[Group, User](group, Map())
    }

    "associate persisted record" in new assoc {
      association.associate(user)
      association.toList mustEqual List(user)
      user.groupId must beSome(group.id)
    }
  }
}

