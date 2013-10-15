package com.github.aselab.activerecord.generator

import sbt._

object Keys {
  lazy val generate = InputKey[Unit]("generate")
  lazy val copyTemplates = TaskKey[Unit]("copyTemplates")
  lazy val templateDirectory = SettingKey[java.io.File]("templateDirectory")
}

