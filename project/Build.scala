import sbt._
import Keys._
import ScctPlugin._
import ls.Plugin._

object ActiveRecordBuild extends Build {
  val _version = "0.2-SNAPSHOT"

  val defaultResolvers = Seq(
    Resolver.sonatypeRepo("snapshots"),
    Classpaths.typesafeReleases
  )

  val defaultSettings: Seq[Project.Setting[_]] = Defaults.defaultSettings ++ Seq(
    version := _version,
    organization := "com.github.aselab",
    scalaVersion := "2.9.2",
    resolvers ++= defaultResolvers,
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    scalacOptions in Compile in doc <++= (baseDirectory).map {base => Seq(
      "-sourcepath", base.getAbsolutePath, "-doc-source-url",
      "https://github.com/aselab/scala-activerecord/tree/master/%s€{FILE_PATH}.scala".format(base.getName)
    )},
    testOptions in ScctTest += Tests.Argument("junitxml", "console"),
    crossPaths := false,
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
    (LsKeys.tags in LsKeys.lsync) := Seq("orm", "db", "database"),
    (externalResolvers in LsKeys.lsync) := defaultResolvers,
    pomIncludeRepository := { _ => false },
    pomExtra := pomXml,
    parallelExecution in ScctTest := false,
    shellPrompt := {
      (state: State) => Project.extract(state).currentProject.id + "> "
    }
  ) ++ lsSettings ++ org.scalastyle.sbt.ScalastylePlugin.Settings ++ ScctPlugin.instrumentSettings

  lazy val core: Project = Project("core", file("activerecord"),
    settings = defaultSettings ++ Seq(
      name := "scala-activerecord",
      libraryDependencies ++= Seq(
        "com.github.aselab" % "squeryl" % "0.9.6-SNAPSHOT",
        "com.github.aselab" % "scala-activerecord-specs" % _version % "test",
        "com.typesafe" % "config" % "1.0.0",
        "org.specs2" %% "specs2" % "1.12.3" % "test",
        "junit" % "junit" % "4.11" % "test",
        "org.mockito" % "mockito-all" % "1.9.5" % "test",
        "com.jolbox" % "bonecp" % "0.7.1.RELEASE",
        "ch.qos.logback" % "logback-classic" % "1.0.9" % "test",
        "com.h2database" % "h2" % "1.3.170" % "test",
        "io.backchat.inflector" %% "scala-inflector" % "1.3.5",
        "com.github.nscala-time" %% "nscala-time" % "0.2.0",
        "commons-validator" % "commons-validator" % "1.4.0"
      ),
      (description in LsKeys.lsync) :=
        "A Scala ORM library like ActiveRecord of Rails.",
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
      libraryDependencies += "org.specs2" %% "specs2" % "1.12.3" % "provided"
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
