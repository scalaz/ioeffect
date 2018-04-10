name := "scalaz-ioeffect"
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core"          % "7.2.21",
  "org.specs2" %% "specs2-core"          % "4.0.0" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "4.0.0" % "test"
)

addCommandAlias("cpl", "all compile test:compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check",
                "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("lint", "all compile:scalafixCli test:scalafixCli")
