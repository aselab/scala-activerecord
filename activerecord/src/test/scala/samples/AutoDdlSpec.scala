package com.github.aselab.activerecord.samples

import org.specs2.mutable._
import org.specs2.specification._

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.models._

object AutoDdlTestTables extends ActiveRecordTables {
  val users = table[User]
  var created = false
  var dropped = false

  override def create = {
    created = true
  }

  override def drop = {
    dropped = true
  }
}

class AutoDdlSpec extends Specification with BeforeEach {
  sequential

  def before = {
    AutoDdlTestTables.created = false
    AutoDdlTestTables.dropped = false
  }

  "ActiveRecordConfig#autoCreate" should {
    "true" in {
      AutoDdlTestTables.initialize(Map("autoCreate" -> true))
      AutoDdlTestTables.cleanup
      AutoDdlTestTables.created should beTrue
    }

    "false" in {
      AutoDdlTestTables.initialize(Map("autoCreate" -> false))
      AutoDdlTestTables.cleanup
      AutoDdlTestTables.created should beFalse
    }
  }

  "ActiveRecordConfig#autoDropped" should {
    "true" in {
      AutoDdlTestTables.initialize(Map("autoDrop" -> true))
      AutoDdlTestTables.cleanup
      AutoDdlTestTables.dropped should beTrue
    }

    "false" in {
      AutoDdlTestTables.initialize(Map("autoDrop" -> false))
      AutoDdlTestTables.cleanup
      AutoDdlTestTables.dropped should beFalse
    }
  }
}
