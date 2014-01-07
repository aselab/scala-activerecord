package com.github.aselab.activerecord.play.sbt

object Plugin extends sbt.Plugin {
  ControllerGenerator.register
  RoutesGenerator.register
  ScaffoldGenerator.register
}
