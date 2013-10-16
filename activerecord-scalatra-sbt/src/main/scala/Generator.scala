package com.github.aselab.activerecord.scalatra

import com.github.aselab.activerecord.generator._

import sbt._
import sbt.complete.DefaultParsers._

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
    val target = sourceDir / "controllers" / (controllerName + ".scala")

    val contents = engine.render("controller/template.ssp", Map(
      ("packageName", "controllers"),
      ("controllerName", controllerName),
      ("actions", actions)
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

