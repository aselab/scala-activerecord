package com.github.aselab.activerecord.samples

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
    ()
  }
}

class SchemaPrefixedSpec extends DatabaseSpecification {
  override def schema = SchemaTestTables

  "Tables.isCreated" should {
    "prefixed tables" >> {
      SchemaTestTables.isCreated must beTrue
    }
  }
}
