val _version = "0.6.2"
val isRelease = System.getProperty("release") == "true"

def specs2(scope: String, name: String = "core") = Def.setting {
  val v = scalaBinaryVersion.value match {
    case "2.13" => "4.10.2"
  }
  "org.specs2" %% s"specs2-${name}" % v % scope
}

def play20(app: String, scope: String) = Def.setting {
  "com.typesafe.play" %% app % "2.8.2" % scope
}

val compileOptions = Seq(
  "-encoding",
  "utf-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wdead-code",
  "-Wvalue-discard",
  "-Xfatal-warnings"
)

val compilerSettings = Seq(
  javacOptions := Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= compileOptions,
  scalacOptions in Compile in doc ++= {
    val base = baseDirectory.value
    Seq("-sourcepath", base.getAbsolutePath, "-doc-source-url",
      "https://github.com/aselab/scala-activerecord/tree/master/%s€{FILE_PATH}.scala".format(base.getName),
      "-diagrams")
  },
  compileOrder in Compile := CompileOrder.JavaThenScala
)

val defaultResolvers = Seq(
  Resolver.sonatypeRepo("snapshots"),
  ("Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases").withAllowInsecureProtocol(true)
)

val defaultSettings = Seq(
  version := (if (isRelease) _version else _version + "-SNAPSHOT"),
  organization := "com.github.aselab",
  scalaVersion := "2.13.3",
  crossScalaVersions := Seq("2.13.3"),
  resolvers ++= defaultResolvers,
  libraryDependencies ++= Seq(
    specs2("test").value,
    specs2("test", "mock").value,
    "com.h2database" % "h2" % "1.4.200" % "test",
    "ch.qos.logback" % "logback-classic" % "1.2.3" % "test"
  ) ++ Option(System.getProperty("ci")).map(_ => specs2("test", "junit").value).toSeq,
  testOptions in Test ++= Option(System.getProperty("ci")).map(_ => Tests.Argument("junitxml", "console")).toSeq,
  parallelExecution in Test := false,
  fork in Test := true,
  testForkedParallel in Test := true,
  publishTo := sonatypePublishToBundle.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := pomXml,
  shellPrompt := {
    (state: State) => Project.extract(state).currentProject.id + "> "
  },
  ivyLoggingLevel := UpdateLogging.DownloadOnly
) ++ compilerSettings ++ org.scalastyle.sbt.ScalastylePlugin.projectSettings

lazy val root = project.in(file("."))
  .settings(defaultSettings: _*)
  .settings(publish := {}, publishLocal := {}, packagedArtifacts := Map.empty)
  .aggregate(macros, core, specs, scalatra, play2, play2Specs)

lazy val core: Project = Project("core", file("activerecord")).settings(defaultSettings:_*).settings(
  name := "scala-activerecord",
  libraryDependencies ++= Seq(
    "org.squeryl" %% "squeryl" % "0.9.15",
    "com.typesafe" % "config" % "1.4.0",
    "com.zaxxer" % "HikariCP" % "3.4.5",
    "com.github.nscala-time" %% "nscala-time" % "2.24.0",
    "commons-validator" % "commons-validator" % "1.7",
    "org.json4s" %% "json4s-native" % "3.6.9",
    "org.slf4j" % "slf4j-api" % "1.7.30",
    "org.scala-lang" % "scalap" % scalaVersion.value
  ),
  unmanagedSourceDirectories in Test += (baseDirectory.value / ".." / "specs" / "src" / "main"),
  initialCommands in console in Test := """
  import com.github.aselab.activerecord._
  import com.github.aselab.activerecord.dsl._
  import models._
  TestTables.initialize(Map("schema" -> "com.github.aselab.activerecord.models.TestTables"))
  """
) dependsOn(macros)

lazy val macros = Project("macro", file("macro")).settings(defaultSettings:_*).settings(
  name := "scala-activerecord-macro",
  libraryDependencies := Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "optional"
  )
)

lazy val specs = project.settings(defaultSettings:_*).settings(
  name := "scala-activerecord-specs",
  libraryDependencies += specs2("provided").value
).dependsOn(core)

lazy val play2 = project.settings(defaultSettings:_*).settings(
  name := "scala-activerecord-play2",
  libraryDependencies ++= List(
    play20("play", "provided").value,
    play20("play-jdbc", "provided").value,
    "com.google.inject" % "guice" % "4.2.3"
  )
).dependsOn(core)

lazy val play2Specs = project.settings(defaultSettings:_*).settings(
  name := "scala-activerecord-play2-specs",
  libraryDependencies ++= List(
    play20("play", "provided").value,
    play20("play-jdbc", "provided").value,
    play20("play-test", "provided").value,
    specs2("provided").value
  )
).dependsOn(play2, specs)

lazy val scalatra = project.settings(defaultSettings:_*).settings(
  name := "scala-activerecord-scalatra",
  libraryDependencies ++= Seq(
    "org.scalatra" %% "scalatra" % "2.7.0" % "provided",
    "javax.servlet" % "javax.servlet-api" % "4.0.1" % "provided",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )
).dependsOn(core)

val pomXml =
  <url>https://github.com/aselab/scala-activerecord</url>
  <licenses>
    <license>
      <name>MIT License</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:aselab/scala-activerecord.git</url>
    <connection>scm:git:git@github.com:aselab/scala-activerecord.git</connection>
  </scm>
  <developers>
    <developer>
      <id>a-ono</id>
      <name>Akihiro Ono</name>
      <url>https://github.com/a-ono</url>
    </developer>
    <developer>
      <id>y-yoshinoya</id>
      <name>Yuki Yoshinoya</name>
      <url>https://github.com/y-yoshinoya</url>
    </developer>
  </developers>
