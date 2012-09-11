resolvers ++= Seq(
  "less is" at "http://repo.lessis.me",
  "coda" at "http://repo.codahale.com",
  "aselab repo" at "http://aselab.github.com/maven/",
  "scct-github-repository" at "http://mtkopone.github.com/scct/maven-repo",
  Classpaths.typesafeResolver
)

addSbtPlugin("me.lessis" % "ls-sbt" % "0.1.2")

addSbtPlugin("reaktor" % "sbt-scct" % "0.2-SNAPSHOT")

addSbtPlugin("com.github.aselab" %% "scalastyle-sbt-plugin" % "0.1.0")

