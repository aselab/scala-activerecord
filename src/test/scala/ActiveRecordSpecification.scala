package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._

import org.slf4j.LoggerFactory
import ch.qos.logback.classic._

trait ActiveRecordSpecification extends Specification {
  sequential

  def logger(name: String) = LoggerFactory.getLogger(name).asInstanceOf[Logger]

  def before = {
    logger(org.slf4j.Logger.ROOT_LOGGER_NAME).setLevel(Level.OFF)
    if (System.getProperty("debug") == "true")
      logger("activerecord").setLevel(Level.DEBUG)

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
