package com.github.aselab.activerecord.sbt

case class Field(
  name: String,
  className: String,
  isOption: Boolean,
  annotations: Seq[String] = Nil
) {
  val shortClassName = className.split("\\.").last

  val typeName = if (isOption) {
    s"Option[${shortClassName}]"
  } else {
    shortClassName
  }

  val importStatement = if (className != shortClassName) {
    Some("import " + className)
  } else {
    None
  }

  val declaration = s"${name}: ${typeName}"

  val scalaDoc = s"@param ${name} "

  override def toString = (annotations :+ declaration).mkString("\n  ")
}

object Field {
  def apply(field: Seq[String]): Field = field match {
    case Seq(name, fieldType, options @ _*) =>
      val cname = types(fieldType.toLowerCase)
      val (opt, annotations) = options.map(_.toLowerCase).partition(_ == "option")
      Field(name, cname, opt.nonEmpty, annotations.map(s => "@" + s.capitalize))
    case _ => sys.error("parse error")
  }

  val types = Map(
    "string" -> "String",
    "text" -> "String",
    "int" -> "Int",
    "long" -> "Long",
    "double" -> "Double",
    "boolean" -> "Boolean",
    "date" -> "java.util.Date"
  )

  val allTypes = types.keys.toList
  val allOptions = List("option", "required", "email", "range")
}
