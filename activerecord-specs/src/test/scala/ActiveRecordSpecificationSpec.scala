package com.github.aselab.activerecord

import com.github.aselab.activerecord.dsl._

package models {
  case class Model1(value: Int) extends ActiveRecord
  object Model1 extends ActiveRecordCompanion[Model1]

  case class Model2(value: Int) extends ActiveRecord
  object Model2 extends ActiveRecordCompanion[Model2]

  object Tables1 extends ActiveRecordTables {
    val models = table[Model1]
  }

  object Tables2 extends ActiveRecordTables {
    val models = table[Model2]
  }
}

object Model1Spec extends ActiveRecordSpecification {
  import models._

  override def config = Map(
    "schema" -> "com.github.aselab.activerecord.models.Tables1",
    "jdbcurl" -> "jdbc:h2:mem:tables1"
  )

  "ActiveRecordSpecification" should {
    "run.mode is test" << {
      System.getProperty("run.mode") mustEqual "test"
    }

    "can override config" << {
      Model1(1).create
      Model1.count mustEqual 1
    }
  }
}

object Model2Spec extends ActiveRecordSpecification {
  import models._

  override def config = Map(
    "schema" -> "com.github.aselab.activerecord.models.Tables2",
    "jdbcurl" -> "jdbc:h2:mem:tables2"
  )

  "ActiveRecordSpecification" should {
    "can override config" << {
      Model2(1).create
      Model2.count mustEqual 1
    }
  }
}
