package com.github.aselab.activerecord.generator

case class ModelInfo(
  name: String,
  typeInfo: (String, Seq[String]),
  options: (Boolean, Seq[String]) = (false, Nil)
) {
  lazy val (typeName, imports) = typeInfo match {
    case (t, i) if isOption => ("Option[%s]".format(t), i)
    case info => info
  }

  lazy val (isOption, annotations) = options

  override def toString =
    if (annotations.isEmpty)
      "%s: %s".format(name, typeName)
    else
      "%s %s: %s".format(annotations.map("@%s".format(_)).mkString(" "), name, typeName)

  def param = "@param %s ".format(name)
}

object ModelInfo {
  def apply(fields: List[List[String]]): Seq[ModelInfo] = {
    fields map {
      case List(name, typeName) =>
        ModelInfo(name, getType(typeName))
      case List(name, typeName, option @ _*) =>
        ModelInfo(name, getType(typeName), getOption(option))
      case _ => throw new Exception("parse error")
    }
  }

  val basicTypes = List("string", "int", "double", "boolean", "long")
  val allTypes = basicTypes ++ List("date", "text")

  def getType(typeName: String) = typeName.trim.toLowerCase match {
    case s if basicTypes.exists(_ == s) => (s.capitalize -> Nil)
    case "date" => ("Date" -> Seq("java.util.Date"))
    case "text" => ("String" -> Nil)
  }

  val allOptions = List("option", "required", "email", "range")

  def getOption(options: Seq[String]) = {
    val opts = options.map(_.trim.toLowerCase)
    (opts.exists(_ == "option"), opts.diff(Seq("option")).map(_.capitalize))
  }
}

