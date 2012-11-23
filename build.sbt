name := "scala-activerecord"

version := "0.2-SNAPSHOT"

organization := "com.github.aselab"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "com.github.aselab" % "squeryl" % "0.9.6-SNAPSHOT",
  "com.typesafe" % "config" % "1.0.0",
  "org.specs2" %% "specs2" % "1.12.3" % "test",
  "junit" % "junit" % "4.10" % "test",
  "org.mockito" % "mockito-all" % "1.9.0" % "test",
  "com.jolbox" % "bonecp" % "0.7.1.RELEASE",
  "ch.qos.logback" % "logback-classic" % "1.0.7" % "test",
  "com.h2database" % "h2" % "1.3.168" % "test",
  "io.backchat.inflector" %% "scala-inflector" % "1.3.4",
  "org.scalaj" %% "scalaj-time" % "0.6",
  "commons-validator" % "commons-validator" % "1.4.0"
)

resolvers ++= Seq(
  "aselab" at "http://aselab.github.com/maven/",
  Classpaths.typesafeResolver
)

scalacOptions ++= Seq("-deprecation", "-unchecked")

scalacOptions in Compile in doc <++= (baseDirectory).map {base => Seq(
  "-sourcepath", base.getAbsolutePath, "-doc-source-url",
  "https://github.com/aselab/scala-activerecord/tree/master€{FILE_PATH}.scala"
)}

testOptions in ScctTest += Tests.Argument("junitxml", "console")

crossPaths := false

parallelExecution in Test := false

compileOrder in Compile := CompileOrder.JavaThenScala

publishTo := Some(Resolver.file("file", file("target/publish")))

publish <<= (publish, name).map {(_, name) =>
  val script = Path.userHome / ".sbt/publish"
  if (script.exists)
    "%s %s %s".format(script.getAbsolutePath, file("target/publish").getAbsolutePath, name) !
}

ScctPlugin.instrumentSettings

parallelExecution in ScctTest := false

org.scalastyle.sbt.ScalastylePlugin.Settings

lsSettings

(LsKeys.tags in LsKeys.lsync) := Seq("orm", "db", "database")

(externalResolvers in LsKeys.lsync) := Seq(
  "aselab" at "http://aselab.github.com/maven/",
  "typesafe" at "http://repo.typesafe.com/typesafe/releases/"
)

(description in LsKeys.lsync) :=
  "A Scala ORM library like ActiveRecord of Rails."

initialCommands in console in Test := """
import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import models._
TestTables.initialize(Map("schema" -> "com.github.aselab.activerecord.models.TestTables"))
"""
