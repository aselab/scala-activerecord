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
    System.setProperty("run.mode", "test")
    logger(org.slf4j.Logger.ROOT_LOGGER_NAME).setLevel(Level.OFF)
    if (System.getProperty("debug") == "true")
      logger("activerecord").setLevel(Level.DEBUG)
    schema.initialize(config)
    schema.reset
  }

  def afterAll = dsl.transaction {
    schema.cleanup
    System.clearProperty("run.mode")
  }

  def config: Map[String, String] = Map(
    "schema" -> "com.github.aselab.activerecord.models.TestTables"
  )

  lazy val schema: ActiveRecordTables = try {
    reflections.ReflectionUtil.classToCompanion(config("schema"))
      .asInstanceOf[ActiveRecordTables]
  } catch {
    case e => throw new RuntimeException("cannot load schema class %s")
  }
}
