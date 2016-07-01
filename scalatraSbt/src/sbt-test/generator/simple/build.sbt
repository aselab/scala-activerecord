val _version = Option(System.getProperty("version")).getOrElse(
  throw new RuntimeException("The system property 'version' is not defined.")
)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.github.aselab" %% "scala-activerecord" % _version,
  "com.github.aselab" %% "scala-activerecord-scalatra" % _version,
  "com.h2database" % "h2" % "1.4.192",
  "org.scalatra" %% "scalatra" % "2.4.0",
  "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided"
)

activerecordScalatraSettings
