package com.github.aselab.activerecord.sbt

import com.github.aselab.sbt.Plugin._

object Plugin extends sbt.Plugin {
  val activerecordGeneratorSettings = generatorSettings ++ Seq(
    GeneratorKeys.generators += ModelGenerator
  )
}
