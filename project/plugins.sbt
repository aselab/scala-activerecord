resolvers ++= Seq(
  "less is" at "http://repo.lessis.me",
  "coda" at "http://repo.codahale.com",
  "scct-github-repository" at "http://mtkopone.github.com/scct/maven-repo",
  "sonatype-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  Classpaths.typesafeResolver
)

addSbtPlugin("me.lessis" % "ls-sbt" % "0.1.2")

addSbtPlugin("reaktor" % "sbt-scct" % "0.2-SNAPSHOT")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.2.0")

