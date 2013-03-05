package com.github.aselab.activerecord.squeryl

import com.github.aselab.activerecord.dsl._
import com.github.nscala_time.time.Imports._
import java.sql.Timestamp
import java.util.Date

trait DateTimeSupport { self: org.squeryl.PrimitiveTypeMode =>
  implicit val jodaTimeTEF = new NonPrimitiveJdbcMapper[Timestamp, DateTime, TTimestamp](timestampTEF, this) {
    def convertFromJdbc(t: Timestamp) = new DateTime(t)
    def convertToJdbc(t: DateTime) = new Timestamp(t.getMillis)
  }

  implicit val optionJodaTimeTEF =
    new TypedExpressionFactory[Option[DateTime], TOptionTimestamp]
      with DeOptionizer[Timestamp, DateTime, TTimestamp, Option[DateTime], TOptionTimestamp] {

    val deOptionizer = jodaTimeTEF
  }

  implicit def jodaTimeToTE(s: DateTime) = jodaTimeTEF.create(s)

  implicit def optionJodaTimeToTE(s: Option[DateTime]) = optionJodaTimeTEF.create(s)
}

trait LocalDateSupport { self: org.squeryl.PrimitiveTypeMode =>
  implicit val localDateTEF = new NonPrimitiveJdbcMapper[Date, LocalDate, TDate](dateTEF, this) {
    def convertFromJdbc(d: Date): LocalDate = LocalDate.fromDateFields(d)
    def convertToJdbc(d: LocalDate) = d.toDate
  }

  implicit val optionLocalDateTEF =
    new TypedExpressionFactory[Option[LocalDate], TOptionDate]
      with DeOptionizer[Date, LocalDate, TDate, Option[LocalDate], TOptionDate] {

    val deOptionizer = localDateTEF
  }

  implicit def localDateToTE(s: LocalDate) = localDateTEF.create(s)

  implicit def optionLocalDateToTE(s: Option[LocalDate]) = optionLocalDateTEF.create(s)
}

