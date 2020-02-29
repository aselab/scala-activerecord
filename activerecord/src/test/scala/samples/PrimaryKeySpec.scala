package com.github.aselab.activerecord.samples

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import java.util.UUID

// TODO: support user managed id and immutability
abstract class UUIDRecord extends ActiveRecordBase[UUID] {
  var id: UUID = UUIDRecord.initialValue
  def isPersisted: Boolean = id != UUIDRecord.initialValue

  override def beforeCreate(): Unit = {
    id = UUID.randomUUID
  }
}

object UUIDRecord {
  val initialValue = new UUID(0, 0)
}

case class UUIDModel(var value: String) extends UUIDRecord

object UUIDModel extends ActiveRecordBaseCompanion[UUID, UUIDModel]

object PrimaryKeyTables extends ActiveRecordTables {
  val uuidModels = table[UUIDModel]
}

class PrimaryKeySpec extends ActiveRecordSpecification {
  override def schema = PrimaryKeyTables

  "primary key type" should {
    "uuid" >> {
      val m = UUIDModel("value").create
      m.value = "test"
      m.save must beTrue
      UUIDModel.where(_.value === m.value).toList mustEqual List(m)
    }
  }
}
