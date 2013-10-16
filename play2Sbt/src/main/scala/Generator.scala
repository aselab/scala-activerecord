package com.github.aselab.activerecord.play

import com.github.aselab.activerecord.generator._

import sbt._
import sbt.complete.DefaultParsers._
import mojolly.inflector.InflectorImports._

object ControllerGenerator extends Generator[(String, Seq[Seq[String]])] {
  val name = "controller"

  def generate(args: (String, Seq[Seq[String]])) {
    val (name, actions) = args
    val controllerName = name.capitalize
    val target = sourceDir / "controllers" / (controllerName.pluralize.titleize + ".scala")

    val contents = render("controller/template.ssp", Map(
      ("packageName", "controllers"),
      ("controllerName", controllerName.pluralize.titleize),
      ("modelName", controllerName.singularize.titleize),
      ("instanceName", controllerName.singularize.camelize)
    ))

    IOUtil.save(target, contents)
  }

  val help = "[controllerName] [action]*"

  val argumentsParser = (token(NotSpace, "controllerName") ~ actions)

  lazy val actions = (token(Space) ~> (path ~ action).map{
    case (x ~ y) => List(x, y)
  }).* <~ SpaceClass.*

  lazy val path = token(Field <~ token(':'), "path:action   e.g.) /index:get")
  lazy val action = token(Field).examples("get", "post", "update", "delete")

}

object RoutesGenerator extends Generator[String] {
  val name = "routes"

  def generate(name: String) {
    val target = file("./conf/routes")
    val parser = new Parser.PlayRoute(target)
    parser.insertedContents(name, engine).foreach(IOUtil.save(target, _))
  }

  val help = "[ModelName]"

  val argumentsParser = token(NotSpace, "modelName")
}

