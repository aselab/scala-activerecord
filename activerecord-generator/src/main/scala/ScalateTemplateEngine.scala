package com.github.aselab.activerecord.generator

import sbt._
import org.fusesource.scalate._

class ScalateTemplateEngine(libraryJar: File, baseTemplateDir: File) {

  lazy val engine = {
    val engine = new TemplateEngine(Nil, System.getProperty("scalate.mode", "production"))
    engine.combinedClassPath = true
    if (libraryJar != null)
      engine.classpath = libraryJar.getAbsolutePath
    engine
  }

  def render(targetFile: String, attributes: Map[String, Any]) = {
    val file = baseTemplateDir / targetFile
    val source =
      if (file.exists)
        TemplateSource.fromFile(file, file.getName)
      else
        TemplateSource.fromURL(getClass.getResource("/templates/" + targetFile))
    engine.layout(source, attributes)
  }
}

