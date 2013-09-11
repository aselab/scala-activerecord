package com.github.aselab.activerecord.samples

import org.specs2.mutable._

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.models._

object SchemaTestTables extends ActiveRecordTables {
  val users = table[User]("users", "test_schema")

  override def create = {
    execute("create schema test_schema")
    super.create
  }

  override def drop = {
    super.drop
    execute("drop schema test_schema")
  }
}

object SchemaPrefixedSpec extends DatabaseSpecification {
  override lazy val schema = SchemaTestTables

  "Tables.isCreated" should {
    "prefixed tables" in {
      schema.isCreated must beTrue
    }
  }
}
