name := "scala-activerecord"

version := "0.1-SNAPSHOT"

organization := "com.github.aselab"

libraryDependencies ++= Seq(
  "com.github.aselab" %% "squeryl" % "0.9.5-SNAPSHOT",
  "com.typesafe.config" % "config" % "0.3.0",
  "org.specs2" %% "specs2" % "1.9" % "test",
  "com.jolbox" % "bonecp" % "0.7.1.RELEASE",
  "org.slf4j" % "slf4j-nop" % "1.6.4" % "test",
  "com.h2database" % "h2" % "1.3.157" % "test"
)

resolvers ++= Seq(
  "aselab" at "http://aselab.github.com/maven/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions ++= Seq("-deprecation", "-unchecked")

crossPaths := false

publishTo := Some(Resolver.file("file", file("target/publish")))

