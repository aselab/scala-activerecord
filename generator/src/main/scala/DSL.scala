package com.github.aselab.activerecord.generator

import sbt._
import collection.JavaConversions.enumerationAsScalaIterator
import scala.util.matching.Regex
import java.util.regex.Pattern

trait DSL {
  def logger: Logger
  def engine: ScalateTemplateEngine

  def createDirectory(dir: File): String = {
    val status = if (dir.isDirectory) {
      "exist"
    } else {
      IO.createDirectory(dir)
      "create"
    }
    log(status, dir)
  }

  def createFile(file: File, data: => String): String = {
    val d = data
    val status = if (file.isFile) {
      if (IO.read(file) == d) "identical" else "conflict"
    } else {
      val dir = file.getParentFile
      if (!dir.isDirectory) createDirectory(dir)
      IO.write(file, d)
      "create"
    }
    log(status, file)

    if (status == "conflict") resolveConflict(file, d) else status
  }

  def copyFile(source: File, destination: File): String =
    createFile(destination, IO.read(source))

  def template(destination: File, resource: String, args: Map[String, Any]): String =
    createFile(destination, engine.render(resource, args))

  def prependFile(file: File, data: => String): String =
    insertFileAfter(file, "\\A", data)

  def appendFile(file: File, data: => String): String =
    insertFileBefore(file, "\\z", data) 

  def insertFileBefore(file: File, regex: String, data: => String): String =
    insert(file, regex, data, after = false)

  def insertFileAfter(file: File, regex: String, data: => String): String =
    insert(file, regex, data, after = true)

  private def insert(file: File, regex: String, data: => String,
    after: Boolean): String = {
    val d = data
    val content = IO.read(file)
    val (searchPattern, replacement) = if (after) {
      ((regex + Pattern.quote(d)).r, "$0" + Regex.quoteReplacement(data))
    } else {
      ((Pattern.quote(d) + regex).r, Regex.quoteReplacement(data) + "$0")
    }

    val status = if (searchPattern.findFirstIn(content).isEmpty) {
      val replaced = content.replaceFirst(regex, replacement)
      if (replaced != content) {
        IO.write(file, replaced)
        "insert"
      } else {
        "skip"
      }
    } else {
      "identical"
    }

    log(status, file)
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

  def resolveConflict(file: File, data: String): String = {
    val question = "The file %s exists, do you want to overwrite it? (y/n): ".format(file.getPath)
    def ask: String = {
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

  def log(status: String, file: File): String = {
    logger.info(status + ": " + file)
    status
  }
}
