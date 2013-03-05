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
    val today = LocalDate.now
    def jodaTimeModel(m: Int = 0) =
      JodaTimeModel(now + m.month, Option(tomorrow + m.month),
        today + m.month, Option(tomorrow.toLocalDate + m.month))

    "save" >> {
      val d1 = jodaTimeModel().create
      JodaTimeModel.toList mustEqual List(d1)
    }

    "where" >> {
      val d1 = jodaTimeModel().create
      val d2 = jodaTimeModel(1).create
      val d3 = jodaTimeModel(2).create
      JodaTimeModel.where(_.datetime > DateTime.tomorrow)
        .orderBy(_.datetime).toList must contain(d3, d2).only
      JodaTimeModel.where(_.optDatetime < DateTime.tomorrow).toList must contain(d1).only
      JodaTimeModel.where(_.localDate <= LocalDate.tomorrow + 1.month)
        .orderBy(_.localDate).toList must contain(d2, d1).only
      JodaTimeModel.where(_.optLocalDate > LocalDate.tomorrow).toList must contain(d2, d3).only
    }
  }
}
