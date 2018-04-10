val ioeffect = project.settings(
  name := "scalaz-ioeffect",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.21"
)

// scalaz 7.2 convention has separate test projects...
val test = project
  .settings(
    name := "scalaz-ioeffect-test",
    skip in publish := true,
    libraryDependencies ++= List(
      "org.specs2" %% "specs2-core"          % "4.0.0" % "test",
      "org.specs2" %% "specs2-matcher-extra" % "4.0.0" % "test"
    )
  )
  .dependsOn(ioeffect)

// root project
skip in publish := true

addCommandAlias("cpl", "all compile test:compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check",
                "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("lint", "all compile:scalafixCli test:scalafixCli")
