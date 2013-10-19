package com.github.aselab.activerecord.play

import com.github.aselab.activerecord.generator._

import sbt._
import sbt.complete.DefaultParsers._
import mojolly.inflector.InflectorImports._

object ControllerGenerator extends Generator[(String, Seq[String])] {
  val name = "controller"
  val help = "ControllerName [actions]*"

  val allActions = Seq("index", "show", "create", "update", "delete", "*")

  def generate(args: (String, Seq[String])) {
    val (name, actions) = args
    val className = name.pascalize
    val target = sourceDir / "controllers" / (className + ".scala")

    template(target, "controllers/controller.ssp", Map(
      "className" -> className,
      "actions" -> actions
    ))

    RoutesGenerator.invoke(args)
  }

  val argumentsParser = token(NotSpace, "ControllerName") ~ actions

  lazy val actions = (token(Space) ~> token(NotSpace).examples(allActions:_*)).*
}

object RoutesGenerator extends Generator[(String, Seq[String])] {
  val name = "routes"
  val help = "ControllerName [actions]*"

  def generate(args: (String, Seq[String])) {
    val (name, actions) = args
    val f = file("conf/routes")
    val content = engine.render("conf/routes.ssp", Map(
      "path" -> name.underscore,
      "controllerName" -> name.pascalize,
      "actions" -> actions
    ))
    if (f.exists) {
      val regex = "^(:?[ \t]*#[^\n]*\n)*"
      insertFileAfter(f, regex, content)
    } else {
      createFile(f, content)
    }
  }

  val argumentsParser = ControllerGenerator.argumentsParser
}

object ScaffoldGenerator extends Generator[(String, Seq[Seq[String]])] {
  val name = "scaffold"
  val help = ModelGenerator.help

  def generate(args: (String, Seq[Seq[String]])) {
    val (name, fields) = args
    val controller = name.pascalize.pluralize
    val model = name.pascalize.singularize

    template(sourceDir / "controllers" / (controller + ".scala"),
      "controllers/scaffold.ssp", Map(
        "Controller" -> controller,
        "Model" -> model
      )
    )

    RoutesGenerator.invoke((controller, Seq("*")))
    ModelGenerator.invoke(args)
  }

  val argumentsParser = ModelGenerator.argumentsParser
}
