package com.github.aselab.activerecord

import com.github.aselab.activerecord._
import java.util.{Date, UUID, TimeZone}
import java.sql.Timestamp

object ConverterSpec extends TimeZoneSpec {
  "FormConverter" should {
    "String" in {
      val converter = FormConverter.get(classOf[String]).get
      converter.serialize("str") mustEqual "str"
      converter.deserialize("str") mustEqual "str"
    }

    "Date" in {
      val converter = FormConverter.get(classOf[Date]).get
      val serialized = "1970-01-06T00:00:00.000Z"
      val deserialized = new Date(5L * 1000 * 60 * 60 * 24)
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "Timestamp" in {
      val converter = FormConverter.get(classOf[Timestamp]).get
      val serialized = "1970-01-01T00:00:00.005Z"
      val deserialized = new Timestamp(5L)
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
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
