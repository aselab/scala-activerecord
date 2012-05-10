package com.github.aselab.activerecord

import java.util.{Date, UUID}
import java.sql.Timestamp
import org.scala_tools.time.Imports._
import org.joda.time.format.ISODateTimeFormat

trait IO { this: ActiveRecordBase[_] =>
  import ReflectionUtil._

  def toMap: Map[String, Any] = {
    _companion.formatFields.flatMap { f =>
      val name = f.getName
      (this.getValue[Any](name) match {
        case v: Option[_] => v
        case v => Some(v)
      }).map(name -> _)
    }.toMap
  }

  def assign(data: Map[String, Any]) = {
    import ReflectionUtil._
    data.foreach{case (k, v) =>
      val info = _companion.fieldInfo(k)
      val value = if (info.isOption) Some(v) else v
      this.setValue(k, value)
    }
  }

  def assignFormValues(data: Map[String, String]) = {
    assign(_companion.fieldInfo.flatMap {
      case (name, info) =>
        val converter = Converter.get(info.fieldType).getOrElse(ActiveRecordException.unsupportedType(name))
        val v = data.get(name)
        try {
          if (info.isSeq) {
            val keys = Stream.from(0).map("%s[%d]".format(name, _)).takeWhile(data.isDefinedAt)
            Option(name -> keys.map(key => converter(data(key))).toList)
          } else if (info.isOption && v.get.isEmpty) {
            None
          } else if (info.required && v.get.isEmpty) {
            this.errors.add(name, "is required")
            None
          } else {
            Option(name -> converter(v.get))
          }
        } catch {
          case e =>
            this.errors.add(name, "is invalid")
            None
        }
    })
  }
}

object Converter {
  private val converters = collection.mutable.Map[Class[_], (String) => Any](
    classOf[String] -> {s: String => s},
    classOf[java.lang.Integer] -> {s: String => s.toInt},
    classOf[java.lang.Long] -> {s: String => s.toLong},
    classOf[java.lang.Double] -> {s: String => s.toDouble},
    classOf[java.lang.Boolean] -> {s: String => s.toBoolean},
    classOf[scala.math.BigDecimal] -> {s: String => BigDecimal(s)},
    classOf[java.lang.Float] -> {s: String => s.toFloat},
    classOf[Timestamp] -> {s: String => new Timestamp(ISODateTimeFormat.dateTime.parseDateTime(s).millis)},
    classOf[UUID] -> {s: String => UUID.fromString(s)},
    classOf[Date] -> {s: String => ISODateTimeFormat.dateTime.parseDateTime(s).toDate}
  )

  def register[T](fieldType: Class[T], converter: (String) => T) = converters += (fieldType -> converter)
  def unregister(fieldType: Class[_]) = converters -= fieldType

  def get(fieldType: Class[_]) = converters.get(fieldType)
}

