package com.github.aselab.activerecord.models

import com.github.aselab.activerecord._
import experimental._
import dsl._
import inner._
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
  val profiles = table[Profile]
  val addresses = table[Address]
  val items = table[Item]

  val people = table[PersonView]("people")
  val students = table[Student]
  val teachers = table[Teacher]

  val timestamps = table[TimestampsModel]
  val datestamps = table[DatestampsModel]
  val optimistics = table[OptimisticModel]

  def createTestData = PrimitiveModel.forceInsertAll(
    (1 to 100).map { i => PrimitiveModel.newModel(i, i > 50) }
  )
}

case class User(name: String, isAdmin: Boolean = false) extends ActiveRecord {
  val groupId: Option[Long] = None
  lazy val group = belongsTo[Group]
  lazy val memberships = hasMany[ProjectMembership]
  lazy val projects = hasManyThrough[Project, ProjectMembership](memberships)
  lazy val profile = hasOne[Profile]
  lazy val address = hasOneThrough[Address, Profile](profile)
}

case class Group(name: String) extends ActiveRecord {
  lazy val users = hasMany[User]
  lazy val adminUsers = hasMany[User](
    conditions = Map("isAdmin" -> true)
  )
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

case class Profile(userId: Option[Long], addressId: Long) extends ActiveRecord {
  lazy val user = belongsTo[User]
  lazy val address = belongsTo[Address]
}

case class Address(country: String, city: String) extends ActiveRecord {
  lazy val profile = hasOne[Profile]
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

object Profile extends ActiveRecordCompanion[Profile]
object Address extends ActiveRecordCompanion[Address]

case class TimestampsModel(name: String) extends ActiveRecord with Timestamps
object TimestampsModel extends ActiveRecordCompanion[TimestampsModel]
case class DatestampsModel(name: String) extends ActiveRecord with Datestamps
object DatestampsModel extends ActiveRecordCompanion[DatestampsModel]

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
    Some(BigDecimal(i)),
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

case class OptimisticModel(var field: String) extends ActiveRecord with Optimistic

object OptimisticModel extends ActiveRecordCompanion[OptimisticModel]

case class ListModel(l1: List[String], l2: List[Int]) extends ActiveModel
object ListModel extends ActiveModelCompanion[ListModel] {
  def create(addErrors: (String, String)*) = {
    val l = ListModel(Nil, Nil)
    addErrors.foreach{ case (k, v) => l.errors.add(k, v) }
    l
  }
}

case class NestModel(
  @Required int: Int,
  list: ListModel
) extends ActiveModel {
  def this() = this(1, ListModel(List("a", "b"), List(1, 2)))
}
object NestModel extends ActiveModelCompanion[NestModel] {
  def create(l: ListModel, addErrors: (String, String)*) = {
    val n = NestModel(0, l)
    addErrors.foreach{ case (k, v) => n.errors.add(k, v) }
    n
  }
}

case class ComplexModel(
  @Required int: Int,
  nest: NestModel,
  nestlist: List[NestModel]
) extends ActiveModel {
  def this() = this(1, new NestModel, List(new NestModel, new NestModel))
}
object ComplexModel extends ActiveModelCompanion[ComplexModel]

case class Item(name: String, price: Int) extends ActiveRecord

object Item extends ActiveRecordCompanion[Item]

/**
 * for Single table inheritance
 */
trait Person extends ActiveRecord with STI {
  val name: String
  val age: Int
}

case class PersonView(name: String, age: Int) extends Person
object PersonView extends ActiveRecordCompanion[PersonView]

trait Children extends Person

case class Student(name: String, age: Int) extends Children
object Student extends ActiveRecordCompanion[Student] with STISupport[Student]

case class Teacher(name: String, age: Int) extends Person
object Teacher extends ActiveRecordCompanion[Teacher] with STISupport[Teacher]
