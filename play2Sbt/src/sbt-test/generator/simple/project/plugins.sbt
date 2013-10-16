val _version = Option(System.getProperty("version")).getOrElse(
  throw new RuntimeException("The system property 'version' is not defined.")
)

addSbtPlugin("com.github.aselab" % "scala-activerecord-play2-sbt" % _version)

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.2.0")
