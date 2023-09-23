// The simplest possible sbt build file is just one line:

scalaVersion := "2.12.15"
// That is, to create a valid sbt build, all you've got to do is define the
// version of Scala you'd like your project to use.

// ============================================================================

// Lines like the above defining `scalaVersion` are called "settings" Settings
// are key/value pairs. In the case of `scalaVersion`, the key is "scalaVersion"
// and the value is "2.12.7"

// It's possible to define many kinds of settings, such as:

name := "newmap-ai"
organization := "newmap.ai"
version := "0.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"


libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.jline" % "jline" % "3.21.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.2.6",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.2.6",
  "com.typesafe.akka" %% "akka-stream" % "2.6.16",
  "com.typesafe.akka" %% "akka-remote" % "2.6.16"
)

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Ywarn-unused")

inThisBuild(
  List(
    scalaVersion := "2.12.15",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)