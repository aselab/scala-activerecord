package com.github.aselab.activerecord.generator

import sbt._
import collection.JavaConversions.enumerationAsScalaIterator

object IOUtil {
  def copyResources(loader: ClassLoader, name: String, dir: File, logger: Logger) {
    loader.getResources(name).foreach(url =>
      url.getProtocol match {
        case "file" =>
          val root = file(url.getPath)
          root.***.get.foreach { src =>
            val dst = dir / src.getPath.drop(root.getPath.size)
            if (src.isDirectory) {
              generateDir(dst, logger)
            } else {
              generate(dst, logger) {
                IO.copyFile(src, dst)
              }
            }
          }
        case "jar" =>
          val conn = url.openConnection.asInstanceOf[java.net.JarURLConnection]
          val entryName = conn.getEntryName
          val jarFile = conn.getJarFile
          jarFile.entries.filter(_.getName.startsWith(entryName)).foreach { e =>
            val fileName = e.getName.drop(entryName.size)
            val target = dir / fileName
            if (e.isDirectory) {
              generateDir(target, logger)
            } else {
              generate(target, logger) {
                IO.transfer(jarFile.getInputStream(e), target)
              }
            }
          }
      }
    )
  }

  def generate(file: File, logger: Logger)(f: => Unit) = {
    def ask: Boolean = {
      val question = "The file %s exists, do you want to overwrite it? (y/n): ".format(file.getPath)
      scala.Console.readLine(question).toLowerCase.headOption match {
        case Some('y') => true
        case Some('n') => false
        case _ => ask
      }
    }

    val action = if (file.exists) {
      if (ask) { f; "overwrite" } else { "skip" }
    } else {
      f; "create"
    }
    logger.info(action + ": " + file)
  }

  def generateDir(dir: File, logger: Logger) {
    if (dir.isDirectory) {
      logger.info("exist: " + dir)
    } else {
      IO.createDirectory(dir)
      logger.info("create: " + dir)
    }
  }

  def save(target: File, contents: String)(implicit logger: Logger) {
    val dir = target.getParentFile
    if (!dir.exists) {
      generateDir(dir, logger)
    }
    generate(target, logger) {
      IO.write(target, contents)
    }
  }
}

