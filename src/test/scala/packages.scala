package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._

import org.squeryl._

package models {
  import com.github.aselab.activerecord.annotations._
  import java.util.{Date, UUID}
  import java.sql.Timestamp

  object DummyTables extends ActiveRecordTables with VersionTable {
    val dummyModels = table[DummyModel]
    val dummyModels2 = table[DummyModel2]

    val users = table[User]
    val groups = table[Group]

    val groupToUsers = oneToMany(groups, users)

    def createTestData = (1 to 100).foreach { i =>
      DummyModel.newModel(i, i > 50).save
    }

    override def cleanup = {
      Session.cleanupResources
    }
  }

  case class User(name: String) extends ActiveRecord {
    val groupId: Option[Long] = None
    lazy val group = belongsTo[Group]
  }

  case class Group(name: String) extends ActiveRecord {
    lazy val users = hasMany[User]
  }

  object User extends ActiveRecordCompanion[User]
  object Group extends ActiveRecordCompanion[Group]

  case class DummyModel(
    @Unique var string: String,
    @Ignore var boolean: Boolean,
    @Ignore var int: Int,
    @Ignore var long: Long,
    @Ignore var float: Float,
    @Ignore var double: Double,
    @Ignore var bigDecimal: BigDecimal,
    @Ignore var timestamp: Timestamp,
    @Ignore var date: Date,
    @Ignore var uuid: UUID,

    var ostring: Option[String],

    @Ignore var oboolean: Option[Boolean],
    @Ignore var oint: Option[Int],
    @Ignore var olong: Option[Long],
    @Ignore var ofloat: Option[Float],
    @Ignore var odouble: Option[Double],
    @Ignore var obigDecimal: Option[BigDecimal],
    @Ignore var otimestamp: Option[Timestamp],
    @Ignore var odate: Option[Date],
    @Ignore var ouuid: Option[UUID]
  ) extends ActiveRecord {
    def this() = this("", false, 0, 0, 0.toFloat, 0.0, BigDecimal(0),
      new Timestamp(0), new Date(0), new UUID(0, 0),
      Some(""), Some(false), Some(0), Some(0L), Some(0.toFloat), Some(0.0),
      Some(BigDecimal(0)), Some(new Timestamp(0)), Some(new Date(0)), None
    )
  }

  object DummyModel extends ActiveRecordCompanion[DummyModel] {
    def newModel(i: Int, none: Boolean = false) = DummyModel(
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
}

trait ActiveRecordSpecification extends Specification {
  sequential

  def before = {
    schema.initialize(config)
  }

  def after = dsl.transaction {
    schema.cleanup
  }

  def config: Map[String, String] = Map(
    "schema" -> "com.github.aselab.activerecord.models.DummyTables"
  )

  def schema: ActiveRecordTables = models.DummyTables

  override def map(fs: => Fragments) = {
    Step {
      before
    } ^ fs ^ Step {
      after
    }
  }
}

