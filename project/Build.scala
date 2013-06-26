import sbt._
import Keys._

object ActiveRecordBuild extends Build {
  val _version = "0.2.2"
  val isRelease = System.getProperty("release") == "true"

  def specs2(key: String, version: String) =
    "org.specs2" %% "specs2" % (
       if (version.startsWith("2.10")) "2.0" else "1.12.3"
     ) % key

  val defaultResolvers = Seq(
    Resolver.sonatypeRepo("releases"),
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
      "junit" % "junit" % "4.11" % "test",
      "org.mockito" % "mockito-all" % "1.9.5" % "test",
      "com.h2database" % "h2" % "1.3.170" % "test",
      "ch.qos.logback" % "logback-classic" % "1.0.9" % "test"
    ),
    libraryDependencies <+= scalaVersion(v => specs2("test", v)),
    javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    scalacOptions in Compile in doc <++= (baseDirectory, scalaVersion).map { (base, version) => Seq(
      "-sourcepath", base.getAbsolutePath, "-doc-source-url",
      "https://github.com/aselab/scala-activerecord/tree/master/%s€{FILE_PATH}.scala".format(base.getName)
    ) ++ (if (version.startsWith("2.10")) Seq("-diagrams") else Nil) },
    testOptions in Test ++= (if (Option(System.getProperty("ci")).isDefined) Seq(Tests.Argument("junitxml", "console")) else Nil),
    parallelExecution in Test := false,
    compileOrder in Compile := CompileOrder.JavaThenScala,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
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
  ) ++ org.scalastyle.sbt.ScalastylePlugin.Settings

  lazy val root: Project = Project("root", file("."))
    .settings(defaultSettings: _*)
    .settings(publish := {}, publishLocal := {})
    .aggregate(core, specs, play2, scalatra)

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
      unmanagedSourceDirectories in Test <++= Seq(scalaSource in Compile in specs).join,
      initialCommands in console in Test := """
      import com.github.aselab.activerecord._
      import com.github.aselab.activerecord.dsl._
      import models._
      TestTables.initialize(Map("schema" -> "com.github.aselab.activerecord.models.TestTables"))
      """
    )
  )

  lazy val specs: Project = Project("specs", file("activerecord-specs"),
    settings = defaultSettings ++ Seq(
      name := "scala-activerecord-specs",
      libraryDependencies <+= scalaVersion(v => specs2("provided", v))
    )
  ) dependsOn(core)

  lazy val play2: Project = Project("play2", file("activerecord-play2"),
    settings = defaultSettings ++ Seq(
      name := "scala-activerecord-play2",
      resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/repo",
      libraryDependencies <++= (scalaVersion) { scalaVersion =>
        scalaVersion match {
          case s if s.startsWith("2.10") => {
            val playVersion = "2.1.0"
            Seq(
              "play" %% "play" % playVersion % "provided",
              "play" %% "play-jdbc" % playVersion % "provided"
            )
          }
          case _ => {
            val playVersion = "2.0.4"
            Seq(
              "play" % "play_2.9.1" % playVersion % "provided"
            )
          }
        }
      }
    )
  ) dependsOn(core)

  lazy val scalatra: Project = Project("scalatra", file("activerecord-scalatra"),
    settings = defaultSettings ++ Seq(
      name := "scala-activerecord-scalatra",
      libraryDependencies ++= Seq(
        "org.scalatra" %% "scalatra" % "2.2.0" % "provided",
        "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "provided" artifacts (Artifact("javax.servlet", "jar", "jar"))
      )
    )
  ) dependsOn(core)

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
