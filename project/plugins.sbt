resolvers ++= Seq(
  Classpaths.typesafeReleases,
  Resolver.url("sbt-plugin-releases",
    new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)
)

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.3.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8")

scalacOptions ++= Seq("-deprecation", "-unchecked")
