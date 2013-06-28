package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.io._
import java.sql.Timestamp
import org.joda.time.format.ISODateTimeFormat
import com.github.nscala_time.time.Imports._

trait JodaTimeSupport { self: org.squeryl.PrimitiveTypeMode =>
  implicit val jodaTimeMapper = new CustomTypeMapper[Timestamp, DateTime](self) {
    def fromJdbc(t: Timestamp) = new DateTime(t)
    def toJdbc(t: DateTime) = new Timestamp(t.getMillis)

    override def formConverter = new FormConverter[DateTime] {
      override def serialize(v: Any): String =
        new DateTime(v).toString(ISODateTimeFormat.dateHourMinuteSecond)
      def deserialize(s: String): DateTime =
        ISODateTimeFormat.dateHourMinuteSecond.parseDateTime(s)
    }

    def defaultValue = DateTime.now
  }

  implicit def jodaTimeToTE(s: DateTime) = jodaTimeMapper.jdbcMapper.create(s)
  implicit def optionJodaTimeToTE(s: Option[DateTime]) = jodaTimeMapper.optionJdbcMapper.create(s)
}

