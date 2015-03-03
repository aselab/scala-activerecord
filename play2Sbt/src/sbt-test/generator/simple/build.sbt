activerecordPlaySettings

val _version = Option(System.getProperty("version")).getOrElse(
  throw new RuntimeException("The system property 'version' is not defined.")
)

resolvers += Resolver.sonatypeRepo("snapshots")

lazy val root = Project("root", file(".")).enablePlugins(PlayScala).settings(
  scalaVersion := "2.11.6",
  javaOptions ++= sys.process.javaVmArguments.filter(
    a => Seq("-Xmx", "-Xms", "-XX").exists(a.startsWith)
  ),
  libraryDependencies ++= Seq(
    "com.github.aselab" %% "scala-activerecord" % _version,
    "com.github.aselab" %% "scala-activerecord-play2" % _version,
    jdbc,
    "com.h2database" % "h2" % "1.4.185"
  )
)

TwirlKeys.templateImports += "com.github.aselab.activerecord.views.dsl._"
