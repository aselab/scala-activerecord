val _version = Option(System.getProperty("version")).getOrElse(
  throw new RuntimeException("The system property 'version' is not defined.")
)

addSbtPlugin("com.github.aselab" % "scala-activerecord-scalatra-sbt" % _version)

resolvers += Resolver.sonatypeRepo("snapshots")
