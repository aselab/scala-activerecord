package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._

import org.squeryl._
import experimental._
import dsl._
import java.sql.Timestamp
import java.util.{Date, UUID, TimeZone}

package models {
  import com.github.aselab.activerecord.annotations._
  object TestTables extends ActiveRecordTables with VersionTable {
    val primitiveModels = table[PrimitiveModel]
    val versionModels = table[VersionModel]

    val users = table[User]
    val groups = table[Group]
    val projects = table[Project]
    val roles = table[Role]
    val projectMemberships = table[ProjectMembership]

    val foos = table[Foo]
    val bars = table[Bar]

    val groupToUsers = oneToMany(groups, users)

    // hasManyThrough
    val projectsToUsers = manyToMany(projects, projectMemberships, users)
    val userToProjectMemberships = oneToMany(users, projectMemberships)
    val projectToProjectMemberships = oneToMany(projects, projectMemberships)
    val roleToProjectMemberships = oneToMany(roles, projectMemberships)

    // hasAndBelongsToMany
    val foosToBars = manyToMany(foos, bars)

    def createTestData = (1 to 100).foreach { i =>
      PrimitiveModel.newModel(i, i > 50).save
    }
  }

  case class User(name: String) extends ActiveRecord {
    val groupId: Option[Long] = None
    lazy val group = belongsTo[Group]
    lazy val projects = hasManyThrough[Project, ProjectMembership]
    lazy val memberships = hasMany[ProjectMembership]
  }

  case class Group(name: String) extends ActiveRecord {
    lazy val users = hasMany[User]
  }

  case class Project(name: String) extends ActiveRecord {
    lazy val users = hasManyThrough[User, ProjectMembership]
    lazy val memberships = hasMany[ProjectMembership]
  }

  case class Role(name: String) extends ActiveRecord {
    lazy val memberships = hasMany[ProjectMembership]
  }

  case class ProjectMembership(roleId: Long) extends IntermediateRecord {
    def id = compositeKey(projectId, userId)
    val projectId: Long = 0
    val userId: Long = 0

    lazy val project = belongsTo[Project]
    lazy val user = belongsTo[User]
    lazy val role = belongsTo[Role]
  }

  case class Foo(name: String) extends ActiveRecord {
    lazy val bars = hasAndBelongsToMany[Bar]
  }

  case class Bar(name: String) extends ActiveRecord {
    lazy val foos = hasAndBelongsToMany[Foo]
  }

  object User extends ActiveRecordCompanion[User]
  object Group extends ActiveRecordCompanion[Group]
  object Project extends ActiveRecordCompanion[Project]
  object Role extends ActiveRecordCompanion[Role]
  object ProjectMembership extends IntermediateRecordCompanion[ProjectMembership]
  object Foo extends ActiveRecordCompanion[Foo]
  object Bar extends ActiveRecordCompanion[Bar]

  case class SeqModel(list: List[Int], seq: Seq[Double])

  case class PrimitiveModel(
    @Unique var string: String,
    var boolean: Boolean,
    var int: Int,
    var long: Long,
    var float: Float,
    var double: Double,
    var bigDecimal: BigDecimal,
    var timestamp: Timestamp,
    var date: Date,
    var uuid: UUID,

    var ostring: Option[String],
    var oboolean: Option[Boolean],
    var oint: Option[Int],
    var olong: Option[Long],
    var ofloat: Option[Float],
    var odouble: Option[Double],
    var obigDecimal: Option[BigDecimal],
    var otimestamp: Option[Timestamp],
    var odate: Option[Date],
    var ouuid: Option[UUID]
  ) extends ActiveRecord

  object PrimitiveModel extends ActiveRecordCompanion[PrimitiveModel] {
    def newModel(i: Int, none: Boolean = false) = PrimitiveModel(
      "string" + i,
      i % 2 == 1,
      i,
      i.toLong,
      i.toFloat,
      i.toDouble,
      BigDecimal(i),
      new Timestamp(i.toLong),
      new Date(i.toLong * 1000 * 60 * 60 * 24),
      new UUID(i.toLong, i.toLong),
      Some("string" + i).filterNot(_ => none),
      Some(i % 2 == 1).filterNot(_ => none),
      Some(i).filterNot(_ => none),
      Some(i.toLong).filterNot(_ => none),
      Some(i.toFloat).filterNot(_ => none),
      Some(i.toDouble).filterNot(_ => none),
      Some(BigDecimal(i)).filterNot(_ => none),
      Some(new Timestamp(i.toLong)).filterNot(_ => none),
      Some(new Date(i.toLong * 1000 * 60 * 60 * 24)).filterNot(_ => none),
      Some(new UUID(i.toLong, i.toLong)).filterNot(_ => none)
    )
  }

  case class VersionModel(
    var string: String,
    var boolean: Boolean,
    var int: Int,
    var optionString: Option[String]
  ) extends ActiveRecord with Versionable

  object VersionModel extends ActiveRecordCompanion[VersionModel]
}

trait ActiveRecordSpecification extends Specification {
  sequential

  def before = {
    schema.initialize(config)
    schema.reset
  }

  def after = dsl.transaction {
    schema.cleanup
  }

  def config: Map[String, String] = Map(
    "schema" -> "com.github.aselab.activerecord.models.TestTables"
  )

  def withRollback[T](f: => T) = dsl.transaction {
    val s = org.squeryl.Session.currentSession
    val result = f
    s.connection.rollback
    result
  }

  def schema: ActiveRecordTables = models.TestTables

  override def map(fs: => Fragments) = {
    Step {
      before
    } ^ fs ^ Step {
      after
    }
  }
}

object supportSpec extends Specification {
  "allClasses" should {
    val f = support.allClasses.apply _

    "String" in {
      f("java.lang.String") == classOf[String] must beTrue
    }

    "Boolean" in {
      f("boolean") == classOf[Boolean] must beTrue
      f("java.lang.Boolean") == classOf[Boolean] must beTrue
      f("scala.Boolean") == classOf[Boolean] must beTrue
    }

    "Int" in {
      f("int") == classOf[Int] must beTrue
      f("java.lang.Integer") == classOf[Int] must beTrue
      f("scala.Int") == classOf[Int] must beTrue
    }

    "Long" in {
      f("long") == classOf[Long] must beTrue
      f("java.lang.Long") == classOf[Long] must beTrue
      f("scala.Long") == classOf[Long] must beTrue
    }

    "Float" in {
      f("float") == classOf[Float] must beTrue
      f("java.lang.Float") == classOf[Float] must beTrue
      f("scala.Float") == classOf[Float] must beTrue
    }

    "Double" in {
      f("double") == classOf[Double] must beTrue
      f("java.lang.Double") == classOf[Double] must beTrue
      f("scala.Double") == classOf[Double] must beTrue
    }

    "BigDecimal" in {
      f("scala.math.BigDecimal") == classOf[BigDecimal] must beTrue
    }

    "Timestamp" in {
      f("java.sql.Timestamp") == classOf[java.sql.Timestamp] must beTrue
    }

    "Date" in {
      f("java.util.Date") == classOf[java.util.Date] must beTrue
    }

    "UUID" in {
      f("java.util.UUID") == classOf[java.util.UUID] must beTrue
    }

    "User defined model class" in {
      f("com.github.aselab.activerecord.models.User") == classOf[models.User] must beTrue
    }

    "unsupported class" in {
      support.allClasses.isDefinedAt("com.github.aselab.activerecord.models.SeqModel") must beFalse
    }

  }
}
