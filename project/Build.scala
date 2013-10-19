import sbt._
import Keys._

object ActiveRecordBuild extends Build {
  val _version = "0.2.4"
  val isRelease = System.getProperty("release") == "true"

  def specs2(scope: String) = Def.setting {
    val v = if (scalaBinaryVersion.value == "2.10") "2.2.2" else "1.12.4.1"
    "org.specs2" %% "specs2" % v % scope
  }

  val compilerSettings = Seq(
    javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
    scalacOptions ++= Seq("-deprecation", "-unchecked") ++
      Some("-feature").filter(_ => scalaBinaryVersion.value == "2.10"),
    scalacOptions in Compile in doc ++= {
      val base = baseDirectory.value
      Seq("-sourcepath", base.getAbsolutePath, "-doc-source-url",
        "https://github.com/aselab/scala-activerecord/tree/master/%s€{FILE_PATH}.scala".format(base.getName)
      ) ++ Some("-diagrams").filter(_ => scalaBinaryVersion.value == "2.10")
    },
    compileOrder in Compile := CompileOrder.JavaThenScala
  )

  val defaultResolvers = Seq(
    Resolver.sonatypeRepo("snapshots"),
    Classpaths.typesafeReleases
  )

  val defaultSettings = Defaults.defaultSettings ++ Seq(
    version := (if (isRelease) _version else _version + "-SNAPSHOT"),
    organization := "com.github.aselab",
    scalaVersion := "2.10.2",
    crossScalaVersions := Seq("2.10.2", "2.9.2"),
    resolvers ++= defaultResolvers,
    libraryDependencies ++= Seq(
      specs2("test").value,
      "org.mockito" % "mockito-all" % "1.9.5" % "test",
      "com.h2database" % "h2" % "1.3.170" % "test",
      "ch.qos.logback" % "logback-classic" % "1.0.9" % "test",
      "junit" % "junit" % "4.11" % "test"
    ),
    testOptions in Test ++= Option(System.getProperty("ci")).map(_ => Tests.Argument("junitxml", "console")).toSeq,
    parallelExecution in Test := false,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    pomExtra := pomXml,
    shellPrompt := {
      (state: State) => Project.extract(state).currentProject.id + "> "
    }
  ) ++ compilerSettings ++ org.scalastyle.sbt.ScalastylePlugin.Settings

  val pluginSettings = defaultSettings ++ ScriptedPlugin.scriptedSettings ++
    Seq(
      sbtPlugin := true,
      crossScalaVersions := Seq("2.10.2"),
      ScriptedPlugin.scriptedBufferLog := false,
      ScriptedPlugin.scriptedLaunchOpts += "-Dversion=" + version.value,
      watchSources ++= ScriptedPlugin.sbtTestDirectory.value.***.get
    )

  lazy val root = project.in(file("."))
    .settings(defaultSettings: _*)
    .settings(publish := {}, publishLocal := {})
    .aggregate(core, specs, play2, scalatra, generator, play2Sbt, scalatraSbt)

  lazy val core: Project = Project("core", file("activerecord"),
    settings = defaultSettings ++ Seq(
      name := "scala-activerecord",
      libraryDependencies ++= Seq(
        "com.github.aselab" %% "squeryl" % "0.9.6-M1",
        "com.typesafe" % "config" % "1.0.0",
        "com.jolbox" % "bonecp" % "0.7.1.RELEASE",
        "io.backchat.inflector" %% "scala-inflector" % "1.3.5",
        "com.github.nscala-time" %% "nscala-time" % "0.2.0",
        "commons-validator" % "commons-validator" % "1.4.0",
        "org.slf4j" % "slf4j-api" % "1.7.2"
      ),
      unmanagedSourceDirectories in Test += (scalaSource in Compile in specs).value,
      initialCommands in console in Test := """
      import com.github.aselab.activerecord._
      import com.github.aselab.activerecord.dsl._
      import models._
      TestTables.initialize(Map("schema" -> "com.github.aselab.activerecord.models.TestTables"))
      """
    )
  )

  lazy val specs = project.settings(defaultSettings:_*).settings(
    name := "scala-activerecord-specs",
    libraryDependencies += specs2("provided").value
  ).dependsOn(core)

  lazy val play2 = project.settings(defaultSettings:_*).settings(
    name := "scala-activerecord-play2",
    resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/repo",
    libraryDependencies ++= {
      scalaBinaryVersion.value match {
        case "2.10" => Seq(
          "play" %% "play" % "2.1.0" % "provided",
          "play" %% "play-jdbc" % "2.1.0" % "provided"
        )
        case _ => Seq("play" % "play_2.9.1" % "2.0.4" % "provided")
      }
    }
  ).dependsOn(core)

  lazy val scalatra = project.settings(defaultSettings:_*).settings(
    name := "scala-activerecord-scalatra",
    resolvers += "Akka Repo" at "http://repo.akka.io/repository",
    libraryDependencies ++= Seq(
      "org.scalatra" %% "scalatra" % "2.2.0" % "provided",
      "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "provided" artifacts (Artifact("javax.servlet", "jar", "jar"))
    )
  ).dependsOn(core)

  lazy val generator = project.settings(pluginSettings:_*).settings(
    name := "scala-activerecord-generator",
    libraryDependencies ++= Seq(
      "org.fusesource.scalate" %% "scalate-core" % "1.6.1",
      "io.backchat.inflector" %% "scala-inflector" % "1.3.5"
    )
  )

  lazy val play2Sbt = project.settings(pluginSettings:_*).settings(
    name := "scala-activerecord-play2-sbt"
  ).dependsOn(generator)

  lazy val scalatraSbt = project.settings(pluginSettings:_*).settings(
    name := "scala-activerecord-scalatra-sbt"
  ).dependsOn(generator)

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
}
