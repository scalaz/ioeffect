import sbt._
import sbt.Keys._

import fommil.SensiblePlugin.autoImport._
import fommil.SonatypePlugin.autoImport._
import sbtdynver.DynVerPlugin.autoImport._
import org.scalafmt.sbt.ScalafmtPlugin, ScalafmtPlugin.autoImport._
import scalafix.sbt.ScalafixPlugin, ScalafixPlugin.autoImport._

object ProjectKeys {
  def KindProjector =
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

  def MonadicFor =
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4")

  def SemanticDB =
    addCompilerPlugin(
      ("org.scalameta" % "semanticdb-scalac" % "4.0.0-M4").cross(CrossVersion.full)
    )

  private val silencerVersion = "0.6"
  def Silencer =
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
      "com.github.ghik" %% "silencer-lib" % silencerVersion % "provided"
    )

  val specs2Version = "4.2.0"

  def extraScalacOptions(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 12)) =>
        Seq(
          "-Ywarn-unused:explicits,patvars,imports,privates,locals,implicits",
          "-opt:l:method,inline",
          "-opt-inline-from:scala.**",
          "-opt-inline-from:scalaz.**"
        )
      case _ =>
        Seq("-Xexperimental")
    }
}

object ProjectPlugin extends AutoPlugin {

  override def requires =
    fommil.SensiblePlugin && fommil.SonatypePlugin && ScalafmtPlugin && ScalafixPlugin
  override def trigger = allRequirements

  val autoImport = ProjectKeys
  import autoImport._

  override def buildSettings =
    Seq(
      organization := "org.scalaz",
      crossScalaVersions := Seq("2.12.6", "2.11.12"),
      scalaVersion := crossScalaVersions.value.head,
      sonatypeGithost := (Github, "scalaz", "ioeffect"),
      sonatypeDevelopers := List("John de Goes"),
      licenses := Seq(
        "BSD3" -> url("https://opensource.org/licenses/BSD-3-Clause")
      ),
      startYear := Some(2017),
      scalafmtConfig := Some(file("project/scalafmt.conf")),
      scalafixConfig := Some(file("project/scalafix.conf"))
    )

  override def projectSettings =
    Seq(
      testFrameworks in Test := Seq(TestFrameworks.Specs2, TestFrameworks.ScalaCheck, TestFrameworks.ScalaTest),
      MonadicFor,
      KindProjector,
      //SemanticDB, // disabling scalafix until 0.6 stabilises
      scalacOptions ++= Seq(
        //"-Yrangepos", // needed by semanticdb
        "-unchecked",
        "-explaintypes",
        "-Ypartial-unification",
        "-Xlog-free-terms",
        "-Xlog-free-types",
        "-Xlog-reflective-calls",
        "-language:higherKinds"
      ),
      scalacOptions ++= extraScalacOptions(scalaVersion.value),
      // WORKAROUND https://github.com/ghik/silencer/issues/7
      scalacOptions -= "-Ywarn-dead-code",
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core"          % "7.2.25",
        "org.specs2" %% "specs2-core"          % specs2Version % "test",
        "org.specs2" %% "specs2-matcher-extra" % specs2Version % "test"
      )
    )
}
