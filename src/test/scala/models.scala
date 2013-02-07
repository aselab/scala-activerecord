package com.github.aselab.activerecord.models

import com.github.aselab.activerecord._
import experimental._
import dsl._
import java.sql.Timestamp
import java.util.{Date, UUID, TimeZone}

object TestTables extends ActiveRecordTables with VersionTable {
  val primitiveModels = table[PrimitiveModel]
  val versionModels = table[VersionModel]
  val annotationModels = table[AnnotationModel]

  val users = table[User]
  val groups = table[Group]
  val projects = table[Project]
  val roles = table[Role]
  val projectMemberships = table[ProjectMembership]
  val foos = table[Foo]
  val bars = table[Bar]
  val bazs = table[Baz]

  def createTestData = (1 to 100).foreach { i =>
    PrimitiveModel.newModel(i, i > 50).save
  }
}

case class User(name: String) extends ActiveRecord {
  val groupId: Option[Long] = None
  lazy val group = belongsTo[Group]
  lazy val memberships = hasMany[ProjectMembership]
  lazy val projects = hasManyThrough[Project, ProjectMembership](memberships)
}

case class Group(name: String) extends ActiveRecord {
  lazy val users = hasMany[User]
}

case class Project(name: String) extends ActiveRecord {
  lazy val memberships = hasMany[ProjectMembership]
  lazy val managerMemberships = hasMany[ProjectMembership](
    conditions = Map("roleId" -> Role.manager.id)
  )
  lazy val developerMemberships = hasMany[ProjectMembership](
    conditions = Map("roleId" -> Role.developer.id)
  )

  lazy val users = hasManyThrough[User, ProjectMembership](memberships)
  lazy val managers = hasManyThrough[User, ProjectMembership](managerMemberships)
  lazy val developers = hasManyThrough[User, ProjectMembership](developerMemberships)

  lazy val groups = hasManyThrough[Group, User](users)
}

case class Role(name: String) extends ActiveRecord {
  lazy val memberships = hasMany[ProjectMembership]
}

case class ProjectMembership(
  projectId: Long = 0, userId: Long = 0, roleId: Option[Long] = None
) extends ActiveRecord {
  lazy val project = belongsTo[Project]
  lazy val user = belongsTo[User]
  lazy val role = belongsTo[Role]
}

case class Foo(name: String) extends ActiveRecord {
  lazy val bars = hasAndBelongsToMany[Bar]
  lazy val bazs = hasAndBelongsToMany[Baz]
}

case class Bar(name: String) extends ActiveRecord {
  lazy val foos = hasAndBelongsToMany[Foo]
}

case class Baz(name: String) extends ActiveRecord {
  lazy val foos = hasAndBelongsToMany[Foo]
}

object User extends ActiveRecordCompanion[User]
object Group extends ActiveRecordCompanion[Group]
object Project extends ActiveRecordCompanion[Project]
object Role extends ActiveRecordCompanion[Role] {
  lazy val manager = all.findByOrCreate(Role("manager"), "name")
  lazy val developer = all.findByOrCreate(Role("developer"), "name")
}
object ProjectMembership extends ActiveRecordCompanion[ProjectMembership]

object Foo extends ActiveRecordCompanion[Foo]
object Bar extends ActiveRecordCompanion[Bar]
object Baz extends ActiveRecordCompanion[Baz]

case class SeqModel(list: List[Int], seq: Seq[Double])

case class PrimitiveModel(
  var string: String,
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
  var odate: Option[Date]
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
    Some(new Date(i.toLong * 1000 * 60 * 60 * 24)).filterNot(_ => none)
  )
}

case class VersionModel(
  var string: String,
  var boolean: Boolean,
  var int: Int,
  var optionString: Option[String]
) extends ActiveRecord with Versionable

object VersionModel extends ActiveRecordCompanion[VersionModel]

case class AnnotationModel(
  @Transient transientField: String,
  @Column("columnName") columnField: String,
  @Unique uniqueField: String,
  @Confirmation confirmationField: String,
  @Confirmation("confirmationName") confirmationField2: String,
  confirmationFieldConfirmation: String,
  confirmationName: String
) extends ActiveRecord

object AnnotationModel extends ActiveRecordCompanion[AnnotationModel]

