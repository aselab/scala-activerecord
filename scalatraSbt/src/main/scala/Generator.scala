package com.github.aselab.activerecord.scalatra.sbt

import com.github.aselab.sbt.Generator
import com.github.aselab.activerecord.sbt._

import sbt._
import sbt.complete.DefaultParsers._
import com.github.aselab.activerecord.util.InflectorImports._

object ControllerGenerator extends Generator[(String, Seq[(String, String)])] {
  val name = "controller"

  def generate(args: (String, Seq[(String, String)])) {
    val (name, actions) = args
    val controllerName = name.pascalize
    val target = sourceDir / "controllers" / (controllerName + ".scala")

    template(target, "controller/template.ssp", Map(
      "packageName" -> "controllers",
      "controllerName" -> controllerName,
      "actions" -> actions
    ))
  }

  val help = "[controllerName] [action]*"

  val argumentsParser = Space ~> token(ScalaID, "controllerName") ~ actions

  lazy val actions = (Space ~> method ~ path).*

  lazy val method = ScalaID.examples("get", "post", "update", "delete") <~ token(':')
  lazy val path = token(NotSpace, "path")
}

