package com.github.aselab.activerecord

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.execute.{AsResult, Result}
import org.specs2.specification.core.Fragments
import scala.language.implicitConversions

/** Runs each example in a transaction that is rolled back afterwards. */
trait AutoRollback extends AroundEach { self: ActiveRecordSpecification =>
  def around[R: AsResult](r: => R): Result = {
    self.schema.foreach(_.startTransaction)
    try AsResult(r) finally self.schema.reverse.foreach(_.rollback)
  }
}

// NoTypedEqual: specs2's `===` matcher would otherwise shadow Squeryl's `===` in query expressions
trait ActiveRecordSpecification extends Specification with BeforeAfterSpec with org.specs2.matcher.NoTypedEqual {
  sequential
  implicit def toTableList(table: ActiveRecordTables): Seq[ActiveRecordTables] = Seq(table)

  def beforeSpec: Fragments = Fragments(fragmentFactory.step(beforeAll()))
  def afterSpec: Fragments = Fragments(fragmentFactory.step(afterAll()))

  /** initializes the schemas before the specification; override to add setup */
  def beforeAll(): Unit = {
    System.setProperty("run.mode", "test")
    schema.foreach(_.initialize(config))
  }

  /** drops the schemas after the specification; override to add teardown */
  def afterAll(): Unit = schema.foreach { s => s.transaction {
    s.drop
    s.cleanup
  }}

  private var factories = Map.empty[String, Map[String, Any]]

  implicit class RichActiveRecordCompanion[T <: ActiveRecordBase[?]](companion: ActiveRecordBaseCompanion[?, T]) {
    def define(name: String, m: Map[String, Any]): Unit = factories += (name -> m)

    def define(name: String, m: (String, Any)*): Unit = define(name, m.toMap)

    def define(m: Map[String, Any]): Unit = define("default", m)

    def define(m: (String, Any)*): Unit = define(m.toMap)

    def create(name: String, m: Map[String, Any]): T = factory(name).apply().unsafeAssign(m).create

    def create(name: String, m: (String, Any)*): T = create(name, m.toMap)

    def create(m: Map[String, Any]): T = create("default", m)

    def create(m: (String, Any)*): T = create(m.toMap)

    def create: T = create()

    def factory(name: String, m: Map[String, Any] = Map()) = {() => companion.newInstance.unsafeAssign(factories(name) ++ m)}
  }

  def config: Map[String, String] = Map()

  def schema: Seq[ActiveRecordTables] = Config.loadSchemas()
}
