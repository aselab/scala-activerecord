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

  def beforeAll = {
    System.setProperty("run.mode", "test")
    schema.initialize(config)
    schema.reset
  }

  def afterAll = dsl.transaction {
    schema.cleanup
    System.clearProperty("run.mode")
  }

  def loglevel: Level = Level.OFF
  def config: Map[String, String] = Map()

  lazy val schema: ActiveRecordTables = new DefaultConfig().schema
}

trait DatabaseSpecification extends ActiveRecordSpecification {
  def logger(name: String) = LoggerFactory.getLogger(name).asInstanceOf[Logger]
  logger(org.slf4j.Logger.ROOT_LOGGER_NAME).setLevel(Level.OFF)
  if (System.getProperty("debug") == "true")
    logger("activerecord").setLevel(Level.DEBUG)
}
