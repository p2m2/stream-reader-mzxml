import sbt.Test
// The simplest possible sbt build file is just one line:

scalaVersion := "2.13.12"
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
  "co.fs2" %% "fs2-core" % "3.9.2",
  "co.fs2" %% "fs2-io" % "3.9.2",
  "co.fs2" %% "fs2-core" % "3.9.2",
  "org.gnieh" %%"fs2-data-xml"%"1.8.0",
  "com.github.marklister" %% "base64" % "0.3.0",
  "com.github.nscala-time" %% "nscala-time" %"2.32.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "org.typelevel" %% "cats-effect-testing-utest" % "1.5.0" % Test,
  "com.lihaoyi" %% "utest" % "0.8.1" % Test,
)

// For all Sonatype accounts created from February 2021
sonatypeCredentialHost := "s01.oss.sonatype.org"
/*
publishTo := {
  if (isSnapshot.value)
    Some("Sonatype Snapshots Nexus" at "https://oss.sonatype.org/content/repositories/snapshots")
  else
    Some("Sonatype Snapshots Nexus" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
}
publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
pomIncludeRepository := { _ => false }
publishMavenStyle := true
*/
publishTo := sonatypePublishToBundle.value
pomIncludeRepository := { _ => false }
publishMavenStyle := true

testFrameworks += new TestFramework("utest.runner.Framework")

assembly / target := file("assembly")
assembly / assemblyJarName := "pack.jar"

Global / onChangedBuildSource := ReloadOnSourceChanges