package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.execute._
import org.specs2.specification._

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
  }

  def afterAll = dsl.transaction {
    schema.drop
    schema.cleanup
  }

  private var factories = Map.empty[String, Map[String, Any]]

  implicit class RichActiveRecordCompanion[T <: ActiveRecordBase[_]](companion: ActiveRecordBaseCompanion[_, T]) {
    def define(name: String, m: Map[String, Any]): Unit = factories += (name -> m)

    def define(name: String, m: (String, Any)*): Unit = define(name, m.toMap)

    def define(m: Map[String, Any]): Unit = define("default", m)

    def define(m: (String, Any)*): Unit = define(m.toMap)

    def create(name: String, m: Map[String, Any]): T = factory(name).apply.assign(m).create

    def create(name: String, m: (String, Any)*): T = create(name, m.toMap)

    def create(m: Map[String, Any]): T = create("default", m)

    def create(m: (String, Any)*): T = create(m.toMap)

    def create: T = create()

    def factory(name: String, m: Map[String, Any] = Map()) = {() => companion.newInstance(factories(name) ++ m)}
  }

  def config: Map[String, String] = Map()

  lazy val schema: ActiveRecordTables = new DefaultConfig(overrideSettings = config).schema
}
