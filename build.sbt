import sbt.Test

//https://repo1.maven.org/maven2/com/github/p2m2/
scalaVersion := "2.13.12"
crossScalaVersions := List("2.13.12")
name := "stream-reader-mzxml"
organization := "com.github.p2m2"

scmInfo := Some(
  ScmInfo(
    url("https://github.com/p2m2/stream-reader-mzxml"),
    "scm:git@github.com:p2m2/stream-reader-mzxml.git"
  )
)
developers := List(
  Developer("ofilangi", "Olivier Filangi", "olivier.filangi@inrae.fr",url("https://github.com/ofilangi"))
)
licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))
homepage := Some(url("https://github.com/p2m2/stream-reader-mzxml"))

//scalacOptions += "-opt:inline,simplify-jumps,compact-locals,nullness-tracking"
/*
ThisBuild / scalacOptions ++= Seq(
  "-release",
  "8",
  "-deprecation",
  "-feature"
)

ThisBuild / compile / javacOptions ++= Seq(
  "-g", // debug symbols
  "--release",
  "8"
)*/

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "4.1.0",
  "com.lucidchart" %% "xtract" % "2.3.0",
  "co.fs2" %% "fs2-core" % "3.11.0",
  "co.fs2" %% "fs2-io" % "3.11.0",
  "co.fs2" %% "fs2-core" % "3.11.0",
  "org.gnieh" %%"fs2-data-xml"%"1.8.0",
  "com.github.marklister" %% "base64" % "0.3.0",
  "com.github.nscala-time" %% "nscala-time" %"3.0.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
  "org.typelevel" %% "cats-effect-testing-utest" % "1.6.0" % Test,
  "com.lihaoyi" %% "utest" % "0.8.5" % Test,
)

testFrameworks += new TestFramework("utest.runner.Framework")

assembly / target := file("assembly")
assembly / assemblyJarName := "pack.jar"

Global / onChangedBuildSource := ReloadOnSourceChanges