package com.github.aselab.activerecord.generator

import sbt._
import sbt.complete.Parser
import sbt.complete.DefaultParsers._
import scala.util.DynamicVariable

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
  def generate(args: ArgumentsType): Unit

  def invoke(args: Any)(implicit context: GeneratorContext) =
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

  def generate(args: (String, Seq[Seq[String]])) {
    val (name, fields) = args
    val modelName = name.capitalize
    val target = sourceDir / "models" / (modelName + ".scala")

    template(target, "model/template.ssp", Map(
      ("packageName", "models"),
      ("modelName", modelName),
      ("fields", ModelInfo(fields))
    ))
  }

  val help = "ModelName fieldName1:type[:options] fieldName2:type[:options]"

  val argumentsParser = (token(NotSpace, "modelName") ~ fields)
  lazy val fields = (token(Space) ~> (fieldName ~ fieldType ~ options).map{
    case (x ~ y ~ z) => (x +: y +: z).toList
  }).* <~ SpaceClass.*

  lazy val fieldName = token(Field <~ token(':'), "fieldName")
  lazy val fieldType = token(Field).examples(ModelInfo.allTypes: _*)
  lazy val options = (token(':') ~> token(Field).examples(ModelInfo.allOptions: _*)).*
}

