package com.github.aselab.activerecord.scalatra.sbt

import com.github.aselab.sbt.Keys.generators
import com.github.aselab.activerecord.sbt.Plugin._

object Plugin extends sbt.Plugin {
  val activerecordScalatraSettings = activerecordGeneratorSettings ++ Seq(
    generators += ControllerGenerator
  )
}
