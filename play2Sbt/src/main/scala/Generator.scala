package com.github.aselab.activerecord.play

import com.github.aselab.activerecord.generator._

import sbt._
import sbt.complete.DefaultParsers._
import mojolly.inflector.InflectorImports._

class ControllerGenerator extends Generator {
  val name = "controller"

  def generate(info: GenerateInfo) {
    import info._
    val (controllerName, actions) = parsed match {
      case (name: String, acts: Seq[_]) =>
        (name.capitalize, acts.map {
          case action: Seq[_] => action.map(_.toString)
        })
    }
    val target = sourceDir / "controllers" / (controllerName.pluralize.titleize + ".scala")

    val contents = engine.render("controller/template.ssp", Map(
      ("packageName", "controllers"),
      ("controllerName", controllerName.pluralize.titleize),
      ("modelName", controllerName.singularize.titleize),
      ("instanceName", controllerName.singularize.camelize)
    ))

    IOUtil.save(target, contents)
  }

  val help = "[controllerName] [action]*"

  override val argumentsParser = (token(NotSpace, "controllerName") ~ actions)

  lazy val actions = (token(Space) ~> (path ~ action).map{
    case (x ~ y) => List(x, y)
  }).* <~ SpaceClass.*

  lazy val path = token(Field <~ token(':'), "path:action   e.g.) /index:get")
  lazy val action = token(Field).examples("get", "post", "update", "delete")

}

class RoutesGenerator extends Generator {
  val name = "routes"

  def generate(info: GenerateInfo) {
    import info._
    val modelName = parsed.asInstanceOf[String]
    val target = file("./conf/routes")
    val parser = new Parser.PlayRoute(target)
    parser.insertedContents(modelName, engine).foreach(IOUtil.save(target, _))
  }

  val help = "[ModelName]"

  override val argumentsParser = token(NotSpace, "modelName")
}

