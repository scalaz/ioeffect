name := "scalaz-ioeffect"

val specs2Version = "4.0.3"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core"          % "7.2.21",
  "org.specs2" %% "specs2-core"          % specs2Version % "test",
  "org.specs2" %% "specs2-matcher-extra" % specs2Version % "test"
)

addCommandAlias("cpl", "all compile test:compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check",
                "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("lint", "all compile:scalafixTest test:scalafixTest")
addCommandAlias("fix", "all compile:scalafixCli test:scalafixCli")
