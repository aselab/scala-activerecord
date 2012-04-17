package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._

import org.squeryl._

object DummyTables extends ActiveRecordTables with VersionTable {
  val dummyModels = table[DummyModel]
  val dummyModels2 = table[DummyModel2]

  def createTestData = (1 to 100).foreach { i =>
    DummyModel.newModel(i, i > 50).save
  }

  override def cleanup = {
    Session.cleanupResources
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
    "schema" -> "com.github.aselab.activerecord.DummyTables"
  )

  def schema: ActiveRecordTables = DummyTables

  override def map(fs: => Fragments) = {
    Step {
      before
    } ^ fs ^ Step {
      after
    }
  }
}

