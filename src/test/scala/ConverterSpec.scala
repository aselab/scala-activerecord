package com.github.aselab.activerecord

import com.github.aselab.activerecord._
import java.util.{Date, UUID}
import java.sql.Timestamp

object ConverterSpec extends ActiveRecordSpecification {
  "Converter" should {
    "StringConverter" in {
      val converter = Converter.get(classOf[String]).get
      converter.serialize("str") mustEqual "str"
      converter.deserialize("str") mustEqual "str"
    }

    "DateConverter" in {
      val converter = Converter.get(classOf[Date]).get
      val serialized = new Date(5L * 1000 * 60 * 60 * 24)
      val deserialized = "1970-01-06T09:00:00.000+09:00"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "TimestampConverter" in {
      val converter = Converter.get(classOf[Timestamp]).get
      val serialized = new Timestamp(5L)
      val deserialized = "1970-01-01T09:00:00.005+09:00"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "UUIDConverter" in {
      val converter = Converter.get(classOf[UUID]).get
      val serialized = new UUID(5L, 5L)
      val deserialized = "00000000-0000-0005-0000-000000000005"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "BigDecimalConverter" in {
      val converter = Converter.get(classOf[BigDecimal]).get
      val serialized = BigDecimal(5)
      val deserialized = "5"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "IntegerConverter" in {
      val converter = Converter.get(classOf[Integer]).get
      val serialized = 456
      val deserialized = "456"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "LongConverter" in {
      val converter = Converter.get(classOf[Long]).get
      val serialized = 45600L
      val deserialized = "45600"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "JavaLongConverter" in {
      val converter = Converter.get(classOf[java.lang.Long]).get
      val serialized = 45600L
      val deserialized = "45600"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "DoubleConverter" in {
      val converter = Converter.get(classOf[Double]).get
      val serialized = 1.5
      val deserialized = "1.5"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "JavaDoubleConverter" in {
      val converter = Converter.get(classOf[java.lang.Double]).get
      val serialized = 1.5
      val deserialized = "1.5"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "FloatConverter" in {
      val converter = Converter.get(classOf[Float]).get
      val serialized = 1.5f
      val deserialized = "1.5"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "FloatConverter" in {
      val converter = Converter.get(classOf[java.lang.Float]).get
      val serialized = 1.5f
      val deserialized = "1.5"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "BooleanConverter" in {
      val converter = Converter.get(classOf[Boolean]).get
      val serialized = true
      val deserialized = "true"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }

    "JavaBooleanConverter" in {
      val converter = Converter.get(classOf[java.lang.Boolean]).get
      val serialized = true
      val deserialized = "true"
      converter.serialize(deserialized) mustEqual serialized
      converter.deserialize(serialized) mustEqual deserialized
    }
  }
}
