package com.github.aselab.activerecord

import sbt._
import Keys._
import generator.Keys._

object Plugin extends sbt.Plugin {
  import generator._

  val generatorSettings = Seq(
    generate <<= Task.generate,
    copyTemplates <<= Task.copyTemplates,
    templateDirectory := baseDirectory.value / "templates"
  )

  object Task {
    def copyTemplates = Def.task {
      val dir = templateDirectory.value
      val dsl = new DSL {
        val logger = streams.value.log
        val engine = null
      }
      dsl.copyResources(getClass.getClassLoader, "templates", dir)
    }

    def generate = Def.inputTask { Generator.parser.parsed match {
      case (name: String, args) =>
        val scalaJar = scalaInstance.value.libraryJar
        val templateDir = templateDirectory.value
        val sourceDir = (scalaSource in Compile).value
        val logger = streams.value.log
        val c = GeneratorContext(scalaJar, templateDir, sourceDir, logger)
        Generator(name).asInstanceOf[Generator[Any]].invoke(args)(c)
    }}
  }

  ModelGenerator.register
}
