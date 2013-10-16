package com.github.aselab.activerecord.generator

import sbt._
import collection.JavaConversions.enumerationAsScalaIterator
import scala.util.matching.Regex.quoteReplacement

trait DSL {
  def logger: Logger
  def engine: ScalateTemplateEngine

  def createDirectory(dir: File) {
    val status = if (dir.isDirectory) {
      "exist"
    } else {
      IO.createDirectory(dir)
      "create"
    }
    log(status, dir)
  }

  def createFile(file: File, data: => String) {
    val status = if (file.isFile) {
      if (IO.read(file) == data) "identical" else "conflict"
    } else {
      val dir = file.getParentFile
      if (!dir.isDirectory) createDirectory(dir)
      IO.write(file, data)
      "create"
    }
    log(status, file)

    if (status == "conflict") resolveConflict(file, data)
  }

  def copyFile(source: File, destination: File) =
    createFile(destination, IO.read(source))

  def template(destination: File, resource: String, args: Map[String, Any]) =
    createFile(destination, engine.render(resource, args))

  def prependFile(file: File, data: => String) {
    insertFileAfter(file, "\\A", data)
  }

  def appendFile(file: File, data: => String) {
    insertFileBefore(file, "\\z", data) 
  }

  def insertFileBefore(file: File, regex: String, data: => String) {
    val replacement = quoteReplacement(data) + "$0"
    replaceFile(file, regex, replacement)
  }

  def insertFileAfter(file: File, regex: String, data: => String) {
    val replacement = "$0" + quoteReplacement(data)
    replaceFile(file, regex, replacement)
  }

  def replaceFile(file: File, regex: String, data: => String) {
    val content = IO.read(file)
    if (!content.contains(data)) {
      IO.write(file, content.replaceFirst(regex, data))
      log("insert", file)
    }
  }

  def copyResources(loader: ClassLoader, name: String, dir: File) {
    loader.getResources(name).foreach(url =>
      url.getProtocol match {
        case "file" =>
          val root = file(url.getPath)
          root.***.get.foreach { src =>
            val dst = dir / src.getPath.drop(root.getPath.size)
            if (src.isDirectory) {
              createDirectory(dst)
            } else {
              copyFile(src, dst)
            }
          }
        case "jar" =>
          val con = url.openConnection.asInstanceOf[java.net.JarURLConnection]
          val entryName = con.getEntryName
          val jarFile = con.getJarFile
          jarFile.entries.filter(_.getName.startsWith(entryName)).foreach { e =>
            val dst = dir / e.getName.drop(entryName.size)
            if (e.isDirectory) {
              createDirectory(dst)
            } else {
              createFile(dst, IO.readStream(jarFile.getInputStream(e)))
            }
          }
      }
    )
  }

  def resolveConflict(file: File, data: String) {
    val question = "The file %s exists, do you want to overwrite it? (y/n): ".format(file.getPath)
    def ask {
      scala.Console.readLine(question).toLowerCase.headOption match {
        case Some('y') =>
          IO.write(file, data)
          log("force", file)
        case Some('n') =>
          log("skip", file)
        case _ => ask
      }
    }
    ask
  }

  def compare(f1: File, f2: File): Boolean =
    IO.readBytes(f1) == IO.readBytes(f2)

  def log(status: String, file: File) =
    logger.info(status + ": " + file)
}
