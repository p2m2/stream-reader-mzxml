
// The simplest possible sbt build file is just one line:

scalaVersion := "2.13.8"
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
  "org.scalaxb" %% "scalaxb" % "1.11.1",
  "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
  "javax.xml.bind" % "jaxb-api" % "2.3.1",
  "co.fs2" %% "fs2-core" % "3.9.2",
  "co.fs2" %% "fs2-io" % "3.9.2",
  "co.fs2" %%"fs2-core"%"3.9.2",
  "org.gnieh" %%"fs2-data-xml"%"1.8.0"
)

Global / onChangedBuildSource := ReloadOnSourceChanges