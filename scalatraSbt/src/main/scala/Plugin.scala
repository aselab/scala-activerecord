package com.github.aselab.activerecord.scalatra

import com.github.aselab.activerecord.Plugin._

object Plugin extends sbt.Plugin {
  val activerecordSettings = generatorSettings

  ControllerGenerator.register
}

