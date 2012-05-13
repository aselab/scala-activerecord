resolvers ++= Seq(
  "less is" at "http://repo.lessis.me",
  "coda" at "http://repo.codahale.com"
)

libraryDependencies ++= Seq(
  Defaults.sbtPluginExtra("me.lessis" % "ls-sbt" % "0.1.1", "0.11.2", "2.9.1")
)
