package com.github.aselab.activerecord.sbt

import sbt._
import sbt.complete.DefaultParsers._
import mojolly.inflector.InflectorImports._
import com.github.aselab.sbt.Generator

object ModelGenerator extends Generator[(String, Seq[Seq[String]])] {
  val name = "model"
  val help = "ModelName [fieldName1:type[:options] fieldName2:type[:options]]"

  def generate(args: (String, Seq[Seq[String]])) {
    val (name, fields) = args
    val modelName = name.pascalize
    val dest = sourceDir / "models" / (modelName + ".scala")

    template(dest, "models/model.ssp", Map(
      "modelName" -> modelName,
      "fields" -> fields.map(Field.apply)
    ))

    TableGenerator.invoke((modelName, None))
  }

  val argumentsParser = Space ~> token(ScalaID, "ModelName") ~ fields
  lazy val fields = (Space ~> (fieldName ~ fieldType ~ options).map{
    case (x ~ y ~ z) => x :: y :: z.toList
  }).*

  lazy val fieldName = token(ID <~ token(':'), "fieldName")
  lazy val fieldType = ID.examples(Field.allTypes: _*)
  lazy val options = (token(':') ~> ID.examples(Field.allOptions: _*)).*
}

object TableGenerator extends Generator[(String, Option[String])] {
  val name = "table"
  val help = "ModelName [tableName]"

  def generate(args: (String, Option[String])) {
    val (modelName, tableName) = args
    val table = tableName.getOrElse(modelName.underscore.pluralize)
    var insertText = """lazy val %s = table[%s]("%s")""".format(
      modelName.camelize.pluralize, modelName.pascalize, table
    )
    val dest = sourceDir / "models" / "Tables.scala"

    if (dest.exists) {
      val regex = "ActiveRecordTables[^\\{]*\\{"
      insertIntoFileAfter(dest, regex, "\n  " + insertText)
    } else {
      template(dest, "models/schema.ssp", Map(
        "insertText" -> insertText
      ))
    }
  }

  val argumentsParser = Space ~> token(ScalaID, "ModelName") ~ (Space ~> token(NotSpace, "tableName")).?
}

