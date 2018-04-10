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

lazy val ioeffect = (project in file("ioeffect"))
  .settings(name := "scalaz-ioeffect")
  .settings(organization := "org.scalaz.ioeffect")
  .settings(description := "The Scalaz IO Monad")
  .settings(scalaVersion := "2.12.5")
  .settings(scalacOptions := commonSettings)
  .settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.21")

lazy val tests = (project in file("test"))
  .settings(name := "scalaz-ioeffect-test")
  .settings(noPublish:_*)
  .dependsOn(ioeffect)

lazy val noPublish = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {}
)