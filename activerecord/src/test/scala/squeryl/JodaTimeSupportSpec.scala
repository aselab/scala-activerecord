package com.github.aselab.activerecord.squeryl

import org.specs2.mutable._
import com.github.nscala_time.time.Imports._
import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import models._

object JodaTimeSupportSpec extends DatabaseSpecification with AutoRollback {
  "JodaTimeSupport" should {
    val now = DateTime.now
    val tomorrow = DateTime.tomorrow

    "save" >> {
      val d1 = JodaTimeModel(now, Option(tomorrow)).create
      JodaTimeModel.toList mustEqual List(d1)
    }

    "where" >> {
      val d1 = JodaTimeModel(now, Option(tomorrow)).create
      val d2 = JodaTimeModel(now + 1.month, Option(tomorrow + 1.month)).create
      val d3 = JodaTimeModel(now + 2.months, Option(tomorrow + 2.months)).create
      JodaTimeModel.where(_.datetime > DateTime.tomorrow).orderBy(_.datetime).toList must contain(d3, d2).only
      JodaTimeModel.where(_.optDatetime < DateTime.tomorrow).toList must contain(d1).only
    }
  }
}
