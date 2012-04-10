name := "scala-activerecord"

version := "0.1-SNAPSHOT"

organization := "com.github.aselab"

libraryDependencies ++= Seq(
  "org.squeryl" %% "squeryl" % "0.9.5-RC1",
  "com.typesafe.config" % "config" % "0.3.0",
  "org.specs2" %% "specs2" % "1.9" % "test",
  "c3p0" % "c3p0" % "0.9.1.2",
  "com.h2database" % "h2" % "1.3.157" % "test"
)

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions ++= Seq("-deprecation", "-unchecked")

crossPaths := false

publishTo := Some(Resolver.file("file", file("target/publish")))

