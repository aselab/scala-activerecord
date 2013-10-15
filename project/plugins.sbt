addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.3.2")

libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
