val _version = "0.7.0"
val isRelease = System.getProperty("release") == "true"

def specs2(scope: String, name: String = "core") = Def.setting {
  "org.specs2" %% s"specs2-${name}" % "4.23.0" % scope
}

val compileOptions = Seq(
  "-encoding",
  "utf-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wvalue-discard",
  "-Xfatal-warnings"
)

val compilerSettings = Seq(
  // Keep producing Java 8 compatible classes, compiled against the Java 8 API
  javacOptions := Seq("--release", "8"),
  scalacOptions ++= compileOptions ++ Seq("-release", "8"),
  Compile / compileOrder := CompileOrder.JavaThenScala
)

val defaultSettings = Seq(
  version := (if (isRelease) _version else _version + "-SNAPSHOT"),
  organization := "com.github.aselab",
  scalaVersion := "3.3.9",
  crossScalaVersions := Seq("3.3.9"),
  libraryDependencies ++= {
    val junit = specs2("test", "junit").value
    Seq(
      specs2("test").value,
      "org.mockito" % "mockito-core" % "5.24.0" % "test",
      "com.h2database" % "h2" % "1.4.200" % "test",
      "ch.qos.logback" % "logback-classic" % "1.2.3" % "test"
    ) ++ Option(System.getProperty("ci")).map(_ => junit).toSeq
  },
  Test / testOptions ++= Option(System.getProperty("ci")).map(_ => Tests.Argument("junitxml", "console")).toSeq,
  Test / parallelExecution := false,
  Test / fork := true,
  Test / testForkedParallel := true,
  // Squeryl generates proxies with cglib, which needs reflective access to java.lang
  Test / javaOptions += "--add-opens=java.base/java.lang=ALL-UNNAMED",
  // Sonatype Central Portal publishing built into sbt (publishSigned, then sonaRelease)
  publishTo := {
    val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
    if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
    else localStaging.value
  },
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { _ => false },
  pomExtra := pomXml,
  shellPrompt := {
    (state: State) => Project.extract(state).currentProject.id + "> "
  }
) ++ compilerSettings

lazy val root = project.in(file("."))
  .settings(defaultSettings)
  .settings(publish := {}, publishLocal := {}, packagedArtifacts := Def.uncached(Map.empty))
  .aggregate(core, specs)

// The play2, play2Specs and scalatra modules are not migrated to Scala 3 yet;
// their sources are kept but they are not part of the build for now.

lazy val core: Project = Project("core", file("activerecord")).settings(defaultSettings).settings(
  name := "scala-activerecord",
  libraryDependencies ++= Seq(
    "org.squeryl" %% "squeryl" % "0.11.0",
    "com.typesafe" % "config" % "1.4.1",
    "com.zaxxer" % "HikariCP" % "3.4.5",
    "com.github.nscala-time" %% "nscala-time" % "2.34.0",
    "commons-validator" % "commons-validator" % "1.7",
    "io.github.json4s" %% "json4s-native" % "4.1.0",
    "org.slf4j" % "slf4j-api" % "1.7.30"
  ),
  Test / unmanagedSourceDirectories += (baseDirectory.value / ".." / "specs" / "src" / "main"),
  Test / console / initialCommands := """
  import com.github.aselab.activerecord._
  import com.github.aselab.activerecord.dsl._
  import models._
  TestTables.initialize(Map("schema" -> "com.github.aselab.activerecord.models.TestTables"))
  """
)

lazy val specs = project.settings(defaultSettings).settings(
  name := "scala-activerecord-specs",
  libraryDependencies += specs2("provided").value
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
