package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.execute._
import org.specs2.specification._

import org.slf4j.LoggerFactory
import ch.qos.logback.classic._

trait AutoRollback extends BeforeAfterExample {
  def schema: ActiveRecordTables

  def before = schema.startTransaction
  def after = schema.rollback
}

trait BeforeAfterAllExamples extends Specification {
  def beforeAll: Unit
  def afterAll: Unit
  override def map(fs: => Fragments) = {
    Step {
      beforeAll
    } ^ fs ^ Step {
      afterAll
    }
  }
}

trait ActiveRecordSpecification extends BeforeAfterAllExamples {
  sequential

  def logger(name: String) = LoggerFactory.getLogger(name).asInstanceOf[Logger]

  def beforeAll = {
    logger(org.slf4j.Logger.ROOT_LOGGER_NAME).setLevel(Level.OFF)
    if (System.getProperty("debug") == "true")
      logger("activerecord").setLevel(Level.DEBUG)

    schema.initialize(config)
    schema.reset
  }

  def afterAll = dsl.transaction {
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
}
