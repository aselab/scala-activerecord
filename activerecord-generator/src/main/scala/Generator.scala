package com.github.aselab.activerecord.generator

import sbt._
import sbt.complete.DefaultParsers._

case class GenerateInfo(
  engine: ScalateTemplateEngine,
  sourceDir: File,
  parsed: Any
)(implicit val logger: Logger)

trait Generator {
  def name: String
  def generate(info: GenerateInfo): Unit
  def help: String
  def argumentsParser: complete.Parser[_] = spaceDelimited("args*")

  def register = Generator.register(this)

  lazy val Field = charClass(s => !s.isWhitespace && !(s == ':')).+.string
}

object Generator {
  val generators = collection.mutable.Map[String, Generator]()
  val parsers = collection.mutable.MutableList[complete.Parser[_]]()

  lazy val allParser = parsers.reduceLeft {(a, b) => a | b}

  def register(generator: Generator) {
    import generator._
    generators += (name -> generator)
    val parser = Space ~> (token(name <~ Space) ~ argumentsParser)
    parsers += parser !!! "Usage: generate %s %s".format(name, help)
  }

  register(new ModelGenerator)
}

class ModelGenerator extends Generator {
  val name = "model"

  def generate(info: GenerateInfo) {
    import info._
    val (modelName, fields) = parsed match {
      case (name: String, fields: List[_]) =>
        (name.capitalize, fields.map{
          case f: List[_] => f.map(_.toString)
        })
    }
    val target = sourceDir / "models" / (modelName + ".scala")

    val contents = engine.render("model/template.ssp", Map(
      ("packageName", "models"),
      ("modelName", modelName),
      ("fields", ModelInfo(fields))
    ))

    IOUtil.save(target, contents)
  }

  val help = "ModelName fieldName1:type[:options] fieldName2:type[:options]"

  override val argumentsParser = (token(NotSpace, "modelName") ~ fields)
  lazy val fields = (token(Space) ~> (fieldName ~ fieldType ~ options).map{
    case (x ~ y ~ z) => (x +: y +: z).toList
  }).* <~ SpaceClass.*

  lazy val fieldName = token(Field <~ token(':'), "fieldName")
  lazy val fieldType = token(Field).examples(ModelInfo.allTypes: _*)
  lazy val options = (token(':') ~> token(Field).examples(ModelInfo.allOptions: _*)).*
}

