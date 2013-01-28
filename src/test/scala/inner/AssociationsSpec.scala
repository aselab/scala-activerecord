package com.github.aselab.activerecord.inner

import org.specs2.mutable._
import org.specs2.specification.Scope
import com.github.aselab.activerecord._
import models._
import dsl._
import ActiveRecord._

object AssociationSpec extends ActiveRecordSpecification {
  trait OneToManyAssociation extends Scope {
    val user = User("user1").create
    val group = Group("group1").create
    lazy val belongsTo = new BelongsToAssociation[User, Group](user, "groupId")
    lazy val hasMany = new HasManyAssociation[Group, User](group, Map(), "groupId")
  }

  trait ManyToManyAssociation extends Scope {
    val user = User("user1").create
    val project = Project("project1").create

    lazy val userMemberships = new HasManyAssociation[User, ProjectMembership](user, Map(), "userId")
    lazy val projectMemberships = new HasManyAssociation[Project, ProjectMembership](project, Map(), "projectId")

    lazy val projectUsers = new HasManyThroughAssociation[Project, User, ProjectMembership](project, projectMemberships, Map(), "userId")
    lazy val userProjects = new HasManyThroughAssociation[User, Project, ProjectMembership](user, userMemberships, Map(), "projectId")
  }

  "BelongsToAssociation" should {
    "associate persisted record" in new OneToManyAssociation {
      belongsTo.associate(group)
      belongsTo.get must beSome(group)
      user.groupId must beSome(group.id)
    }

    "implicit conversions" in new OneToManyAssociation {
      belongsTo.associate(group)
      belongsTo.count mustEqual 1
      belongsTo.headOption must beSome(group)
      belongsTo.where(_.name === "group1").toList mustEqual List(group)
    }
  }

  "HasManyAssociation" should {
    "associate persisted record" in new OneToManyAssociation {
      hasMany.associate(user)
      hasMany.toList mustEqual List(user)
      user.groupId must beSome(group.id)
    }

    "implicit conversions" in new OneToManyAssociation {
      hasMany.associate(user)
      hasMany.headOption must beSome(user)
      hasMany.where(_.name === "user1").headOption must beSome(user)
    }

    "deleteAll" in new OneToManyAssociation {
      val user2 = User("user2").create
      val user3 = User("user3").create
      hasMany := List(user, user2)
      hasMany.deleteAll mustEqual List(user, user2)
      hasMany must beEmpty
      User.exists(_.id === user.id) must beFalse
      User.exists(_.id === user2.id) must beFalse
      User.exists(_.id === user3.id) must beTrue
    }
  }

  "HasManyThroughAssociation" should {
    "associate persisted record" << new ManyToManyAssociation {
      val membership = projectUsers.associate(user)
      projectUsers.toList mustEqual List(user)
      membership.projectId mustEqual project.id
      membership.userId mustEqual user.id
    }

    "implicit conversions" in new ManyToManyAssociation {
      projectUsers << user
      projectUsers.headOption must beSome(user)
      projectUsers.where(_.name === "user1").headOption must beSome(user)
    }
  }
}

