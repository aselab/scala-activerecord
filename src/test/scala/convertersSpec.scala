package com.github.aselab.activerecord

import com.github.aselab.activerecord._
import java.util.{Date, UUID, TimeZone}
import java.sql.Timestamp
import org.scala_tools.time.Imports._
import org.joda.time.format.ISODateTimeFormat

object convertersSpec extends ActiveRecordSpecification {
  "FormConverter" should {
    "String" in {
      val converter = FormConverter.get(classOf[String]).get
      converter.serialize("str") mustEqual "str"
      converter.deserialize("str") mustEqual "str"
    }

    "Date" in {
      val converter = FormConverter.get(classOf[Date]).get

      "convert with the timezone setting when Config.timeZone is null" in {
        val string = "2012-05-06T08:02:11.530Z"
        val date = ISODateTimeFormat.dateTime.parseDateTime(string).toDate
        converter.serialize(date) mustEqual string
        converter.deserialize(string) mustEqual date
      }

      "convert with Config.timeZone" in {
        Config.timeZone = TimeZone.getTimeZone("Asia/Tokyo")
        val string = "2012-06-16T18:23:51.133+09:00"
        val date = ISODateTimeFormat.dateTime.parseDateTime(string).toDate
        val serialized = converter.serialize(date)
        val deserialized = converter.deserialize(string)
        Config.timeZone = null

        serialized mustEqual string
        deserialized mustEqual date
      }
    }

    "Timestamp" in {
      val converter = FormConverter.get(classOf[Timestamp]).get

      "convert with the timezone setting when Config.timeZone is null" in {
        val string = "2012-05-06T08:02:11.530Z"
        val t = new Timestamp(ISODateTimeFormat.dateTime.parseDateTime(string).millis)
        converter.serialize(t) mustEqual string
        converter.deserialize(string) mustEqual t
      }

      "convert with Config.timeZone" in {
        Config.timeZone = TimeZone.getTimeZone("Asia/Tokyo")
        val string = "2012-06-16T18:23:51.133+09:00"
        val t = new Timestamp(ISODateTimeFormat.dateTime.parseDateTime(string).millis)
        val serialized = converter.serialize(t)
        val deserialized = converter.deserialize(string)
        Config.timeZone = null

        serialized mustEqual string
        deserialized mustEqual t
      }
    }

    "UUID" in {
      val converter = FormConverter.get(classOf[UUID]).get
      val serialized = "00000000-0000-0005-0000-000000000005"
      val deserialized = new UUID(5L, 5L)
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "BigDecimal" in {
      val converter = FormConverter.get(classOf[BigDecimal]).get
      val serialized = "5"
      val deserialized = BigDecimal(5)
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "Integer" in {
      val converter = FormConverter.get(classOf[Integer]).get
      val serialized = "456"
      val deserialized = 456
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "Long" in {
      val converter = FormConverter.get(classOf[Long]).get
      val serialized = "45600"
      val deserialized = 45600L
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "JavaLong" in {
      val converter = FormConverter.get(classOf[java.lang.Long]).get
      val serialized = "45600"
      val deserialized = 45600L
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "Double" in {
      val converter = FormConverter.get(classOf[Double]).get
      val serialized = "1.5"
      val deserialized = 1.5
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "JavaDouble" in {
      val converter = FormConverter.get(classOf[java.lang.Double]).get
      val serialized = "1.5"
      val deserialized = 1.5
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "Float" in {
      val converter = FormConverter.get(classOf[Float]).get
      val serialized = "1.5"
      val deserialized = 1.5f
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "Float" in {
      val converter = FormConverter.get(classOf[java.lang.Float]).get
      val serialized = "1.5"
      val deserialized = 1.5f
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "Boolean" in {
      val converter = FormConverter.get(classOf[Boolean]).get
      val serialized = "true"
      val deserialized = true
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "JavaBoolean" in {
      val converter = FormConverter.get(classOf[java.lang.Boolean]).get
      val serialized = "true"
      val deserialized = true
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }
  }
}
