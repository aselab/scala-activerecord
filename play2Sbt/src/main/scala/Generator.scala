package com.github.aselab.activerecord.play.sbt

import com.github.aselab.sbt.Generator
import com.github.aselab.activerecord.sbt._

import sbt._
import sbt.Keys._
import sbt.complete.DefaultParsers._
import com.github.aselab.activerecord.util.InflectorImports._

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

  val argumentsParser = Space ~> token(ScalaID, "ControllerName") ~ actions

  lazy val actions = (Space ~> ScalaID.examples(allActions:_*)).*
}

object RoutesGenerator extends Generator[(String, Seq[String])] {
  val name = "routes"
  val help = "ControllerName [actions]*"

  def generate(args: (String, Seq[String])) {
    val (name, actions) = args
    val f = context(baseDirectory) / "conf" / "routes"
    val content = scalateTemplate.render("conf/routes.ssp", Map(
      "path" -> name.underscore,
      "controllerName" -> name.pascalize,
      "actions" -> actions
    ))
    if (f.exists) {
      val regex = "^(:?[ \t]*#[^\n]*\n)*"
      insertIntoFileAfter(f, regex, content)
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
    val params = Map(
      "controller" -> controller,
      "model" -> model,
      "columns" -> fields.map(_.head)
    )

    template(sourceDir / "controllers" / (controller + ".scala"),
      "controllers/scaffold.ssp", params
    )

    val base = sourceDir / "views" / controller.underscore
    template(base / "index.scala.html", "views/index.ssp", params)
    template(base / "show.scala.html", "views/show.ssp", params)
    template(base / "edit.scala.html", "views/edit.ssp", params)

    RoutesGenerator.invoke((controller, Seq("*")))
    ModelGenerator.invoke(args)
  }

  val argumentsParser = ModelGenerator.argumentsParser
}
