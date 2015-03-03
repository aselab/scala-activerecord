addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.6.0")

libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
