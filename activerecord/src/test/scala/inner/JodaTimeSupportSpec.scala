package com.github.aselab.activerecord.inner

import org.specs2.mutable._
import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.models.DateTimeModel
import com.github.nscala_time.time.Imports._

object JodaTimeSupportSpec extends DatabaseSpecification with AutoRollback {
  "JodaTimeSupport" should {
    "save" in {
      val now = DateTime.now
      val m = DateTimeModel(now)
      m.save() must beTrue
      DateTimeModel.head.datetime mustEqual now
    }

    "where" in {
      val d1 = DateTimeModel(DateTime.now.withYear(1970)).create
      val d2 = DateTimeModel(DateTime.now.withYear(2000)).create
      val d3 = DateTimeModel(DateTime.now.withYear(2100)).create
      val result = DateTimeModel.where(_.datetime < DateTime.now.withYear(2010))
      result.toList mustEqual List(d1, d2)
    }
  }
}

