name := "scalaz-ioeffect"

version := "0.1"

scalaVersion := "2.12.5"

lazy val ioeffect = (project in file("ioeffect"))
  .settings(name := "scalaz-ioeffect")
  .settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.21")