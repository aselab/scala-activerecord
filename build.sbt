name := "scala-activerecord"

version := "0.2-SNAPSHOT"

organization := "com.github.aselab"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "com.github.aselab" % "squeryl" % "0.9.5",
  "com.typesafe" % "config" % "0.4.0",
  "org.specs2" %% "specs2" % "1.9" % "test",
  "com.jolbox" % "bonecp" % "0.7.1.RELEASE",
  "org.slf4j" % "slf4j-nop" % "1.6.4" % "test",
  "com.h2database" % "h2" % "1.3.166" % "test",
  "io.backchat.inflector" %% "scala-inflector" % "1.3.3",
  "org.scala-tools.time" % "time_2.9.1" % "0.5",
  "commons-validator" % "commons-validator" % "1.4.0"
)

resolvers ++= Seq(
  "aselab" at "http://aselab.github.com/maven/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions ++= Seq("-deprecation", "-unchecked")

crossPaths := false

parallelExecution in Test := false

compileOrder := CompileOrder.JavaThenScala

publishTo := Some(Resolver.file("file", file("target/publish")))

publish <<= (publish, name).map {(_, name) =>
  val script = Path.userHome / ".sbt/publish"
  if (script.exists)
    "%s %s %s".format(script.getAbsolutePath, file("target/publish").getAbsolutePath, name) !
}
