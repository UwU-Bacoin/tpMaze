
scalaVersion := "3.3.1"

name := "TPmaze"
organization := "fr.istic.prg2"
version := "1.0"

ThisBuild / scalacOptions ++= Seq("-deprecation")

/* Munit & Scalacheck
https://scalameta.org/munit/docs/integrations/scalacheck.html
 */
libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test

/* https://sttp.softwaremill.com/en/stable/quickstart.html */
libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M11"

libraryDependencies += "com.softwaremill.sttp.client4" %% "circe" % "4.0.0-M11"
libraryDependencies += "io.circe" %% "circe-generic" % "0.15.0-M1"

