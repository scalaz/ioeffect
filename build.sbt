lazy val commonSettings = List(
  // contains -language:postfixOps (because 1+ as a parameter to a higher-order function is treated as a postfix op)
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-Xfuture",
  "-Ypartial-unification",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
  "-unchecked",
  "-Xexperimental" // SAM types in 2.11
)

lazy val root = (project in file ("."))
  .settings(organization := "org.scalaz.ioeffect")
  .settings(description := "The Scalaz IO Monad")
  .settings(scalaVersion := "2.12.5")
  .aggregate(ioeffect, test)


lazy val ioeffect = (project in file("ioeffect"))
  .settings(name := "scalaz-ioeffect")
  .settings(scalacOptions := commonSettings)
  .settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.21")

lazy val test = (project in file("test"))
  .settings(name := "scalaz-ioeffect-test")
  .settings(scalacOptions := commonSettings)
  .settings(noPublish:_*)
  .settings(libraryDependencies ++= List(
      "org.specs2" %% "specs2-core" % "4.0.0" % "test",
      "org.specs2" %% "specs2-matcher-extra" % "4.0.0" % "test"
    )
  )
  .dependsOn(ioeffect)

lazy val noPublish = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {}
)