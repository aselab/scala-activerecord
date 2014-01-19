package com.github.aselab.activerecord.play.sbt

import com.github.aselab.sbt.Keys.generators
import com.github.aselab.activerecord.sbt.Plugin._

object Plugin extends sbt.Plugin {
  val activerecordPlaySettings = activerecordGeneratorSettings ++ Seq(
    generators ++= Seq(
      ControllerGenerator,
      RoutesGenerator,
      ScaffoldGenerator
    )
  )
}
