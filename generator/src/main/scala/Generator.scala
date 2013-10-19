package com.github.aselab.activerecord.generator

import sbt._
import sbt.complete.Parser
import sbt.complete.DefaultParsers._
import scala.util.DynamicVariable
import mojolly.inflector.InflectorImports._

case class GeneratorContext(
  scalaJar: File,
  templateDir: File,
  sourceDir: File,
  logger: Logger
) {
  val engine = new ScalateTemplateEngine(scalaJar, templateDir)
}

trait Generator[ArgumentsType] extends DSL {
  def name: String
  def help: String
  def argumentsParser: Parser[ArgumentsType]
  protected def generate(args: ArgumentsType): Unit

  def invoke(args: ArgumentsType)(implicit context: GeneratorContext) =
    _context.withValue(context) {
      generate(args.asInstanceOf[ArgumentsType])
    }

  def register = Generator.register(this)

  lazy val Field = charClass(s => !s.isWhitespace && !(s == ':')).+.string

  private val _context = new DynamicVariable[GeneratorContext](null)
  def engine = _context.value.engine
  def logger = _context.value.logger
  def sourceDir = _context.value.sourceDir
  implicit protected def context = _context.value
}

object Generator {
  private val generators = collection.mutable.Map[String, Generator[_]]()
  private val parsers = collection.mutable.MutableList[Parser[_]]()

  lazy val parser = parsers.reduceLeft {(a, b) => a | b}

  def apply(name: String): Generator[_] = generators(name)

  def register(generator: Generator[_]) {
    import generator._
    generators += (name -> generator)
    val parser = Space ~> (token(name <~ Space) ~ argumentsParser)
    parsers += parser !!! "Usage: generate %s %s".format(name, help)
  }
}

object ModelGenerator extends Generator[(String, Seq[Seq[String]])] {
  val name = "model"
  val help = "ModelName [fieldName1:type[:options] fieldName2:type[:options]]"

  def generate(args: (String, Seq[Seq[String]])) {
    val (name, fields) = args
    val modelName = name.pascalize
    val dest = sourceDir / "models" / (modelName + ".scala")

    template(dest, "models/model.ssp", Map(
      "modelName" -> modelName,
      "fields" -> ModelInfo(fields)
    ))

    TableGenerator.invoke((modelName, None))
  }

  val argumentsParser = (token(NotSpace, "ModelName") ~ fields)
  lazy val fields = (token(Space) ~> (fieldName ~ fieldType ~ options).map{
    case (x ~ y ~ z) => (x +: y +: z).toList
  }).* <~ SpaceClass.*

  lazy val fieldName = token(Field <~ token(':'), "fieldName")
  lazy val fieldType = token(Field).examples(ModelInfo.allTypes: _*)
  lazy val options = (token(':') ~> token(Field).examples(ModelInfo.allOptions: _*)).*
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
      insertFileAfter(dest, regex, "\n  " + insertText)
    } else {
      template(dest, "models/schema.ssp", Map(
        "insertText" -> insertText
      ))
    }
  }

  val argumentsParser = (token(NotSpace, "ModelName") ~ ((token(Space) ~> token(NotSpace, "tableName")).?))
}

