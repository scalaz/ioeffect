val ioeffect = project.settings(
  name := "scalaz-ioeffect"
)

val `ioeffect-cats` = project
  .settings(
    name := "scalaz-ioeffect-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "0.10",
      "org.typelevel" %% "cats-effect-laws" % "0.10" % "test")
  )
  .dependsOn(ioeffect)

// conveniences
addCommandAlias("cpl", "all compile test:compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check",
                "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("lint", "all compile:scalafixTest test:scalafixTest")
addCommandAlias("fix", "all compile:scalafixCli test:scalafixCli")

// root project
skip in publish := true
