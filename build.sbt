import sbt.Test
// The simplest possible sbt build file is just one line:

scalaVersion := "2.13.12"
// That is, to create a valid sbt build, all you've got to do is define the
// version of Scala you'd like your project to use.

// ============================================================================

// Lines like the above defining `scalaVersion` are called "settings". Settings
// are key/value pairs. In the case of `scalaVersion`, the key is "scalaVersion"
// and the value is "2.13.8"

// It's possible to define many kinds of settings, such as:

name := "mzXML-stream"
organization := "com.github.p2m2"
version := "1.0"


libraryDependencies ++= Seq(
 "com.lucidchart" %% "xtract" % "2.3.0",
  "co.fs2" %% "fs2-core" % "3.9.2",
  "co.fs2" %% "fs2-io" % "3.9.2",
  "co.fs2" %% "fs2-core" % "3.9.2",
  "org.gnieh" %%"fs2-data-xml"%"1.8.0",
  "com.github.marklister" %% "base64" % "0.3.0",
  "org.typelevel" %% "cats-effect-testing-utest" % "1.5.0" % Test,
  "com.lihaoyi" %% "utest" % "0.8.1" % Test,
)
libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case _ ⇒ "1.6.5"
  }
  "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
}

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

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

testFrameworks += new TestFramework("utest.runner.Framework")

Global / onChangedBuildSource := ReloadOnSourceChanges