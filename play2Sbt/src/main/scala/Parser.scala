package com.github.aselab.activerecord.play

import com.github.aselab.activerecord.generator._
import sbt.complete.DefaultParsers._
import java.io._
import scala.io.Source
import mojolly.inflector.InflectorImports._

object Parser {
  class PlayRoute(routesFile: File) {
    val lines = Source.fromFile(routesFile).getLines.toSeq

    def searchInsertPosition = {
      val insertRegex = "^[^#]*".r
      lines.zipWithIndex.collectFirst{ case (insertRegex(), i) => i }
    }

    def alreadyExists(modelName: String) = {
      val searchRegex = """^GET\s+/%s.*""".format(modelName.toLowerCase).r
      lines.collectFirst { case searchRegex() => true }.nonEmpty
    }

    def insertedContents(modelName: String, engine: ScalateTemplateEngine) = {
      val template = engine.render("routes/template.ssp", Map(
        "uri" -> modelName.pluralize.camelize,
        "controllerName" -> modelName.pluralize.titleize
      ))
      if (!alreadyExists(modelName)) {
        searchInsertPosition.map { i =>
          val (l, r) = lines.splitAt(i)
          (l ++ template.split("\\r?\\n") ++ List("") ++ r).mkString("\r\n")
        }
      } else {
        None
      }
    }
  }
}

