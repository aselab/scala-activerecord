package com.github.aselab.activerecord.squeryl

import com.github.aselab.activerecord.dsl._
import com.github.nscala_time.time.Imports._
import java.sql.Timestamp
import java.util.Date
import org.squeryl.dsl.TypedExpression
import scala.language.implicitConversions

trait DateTimeSupport { self: org.squeryl.PrimitiveTypeMode =>
  implicit val jodaTimeTEF: NonPrimitiveJdbcMapper[Timestamp, DateTime, TTimestamp] =
    new NonPrimitiveJdbcMapper[Timestamp, DateTime, TTimestamp](timestampTEF, this) {
    def convertFromJdbc(t: Timestamp) = new DateTime(t)
    def convertToJdbc(t: DateTime) = new Timestamp(t.getMillis)
  }

  implicit val optionJodaTimeTEF: TypedExpressionFactory[Option[DateTime], TOptionTimestamp]
    with DeOptionizer[Timestamp, DateTime, TTimestamp, Option[DateTime], TOptionTimestamp] =
    new TypedExpressionFactory[Option[DateTime], TOptionTimestamp]
      with DeOptionizer[Timestamp, DateTime, TTimestamp, Option[DateTime], TOptionTimestamp] {

    val deOptionizer = jodaTimeTEF
  }

  implicit def jodaTimeToTE(s: DateTime): TypedExpression[DateTime, TTimestamp] = jodaTimeTEF.create(s)

  implicit def optionJodaTimeToTE(s: Option[DateTime]): TypedExpression[Option[DateTime], TOptionTimestamp] =
    optionJodaTimeTEF.create(s)
}

trait LocalDateSupport { self: org.squeryl.PrimitiveTypeMode =>
  implicit val localDateTEF: NonPrimitiveJdbcMapper[Date, LocalDate, TDate] =
    new NonPrimitiveJdbcMapper[Date, LocalDate, TDate](dateTEF, this) {
    def convertFromJdbc(d: Date): LocalDate = LocalDate.fromDateFields(d)
    def convertToJdbc(d: LocalDate) = d.toDate
  }

  implicit val optionLocalDateTEF: TypedExpressionFactory[Option[LocalDate], TOptionDate]
    with DeOptionizer[Date, LocalDate, TDate, Option[LocalDate], TOptionDate] =
    new TypedExpressionFactory[Option[LocalDate], TOptionDate]
      with DeOptionizer[Date, LocalDate, TDate, Option[LocalDate], TOptionDate] {

    val deOptionizer = localDateTEF
  }

  implicit def localDateToTE(s: LocalDate): TypedExpression[LocalDate, TDate] = localDateTEF.create(s)

  implicit def optionLocalDateToTE(s: Option[LocalDate]): TypedExpression[Option[LocalDate], TOptionDate] =
    optionLocalDateTEF.create(s)
}

