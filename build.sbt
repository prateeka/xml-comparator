import sbt.Keys.libraryDependencies

name := "xml-comparator"

version := "0.1"

scalaVersion := "3.1.2"

lazy val xmlVersion = "2.1.0"
lazy val loggingVersion = "3.9.4"
lazy val scallopVersion = "4.1.0"
lazy val scalaTestVersion = "3.2.12"
lazy val snaymlVersion = "1.30"
lazy val logbackClassicVersion = "1.2.11"
lazy val circeYamlVersion = "0.14.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % xmlVersion,
  "com.typesafe.scala-logging" %% "scala-logging" % loggingVersion,
  "ch.qos.logback" % "logback-classic" % logbackClassicVersion,
  "org.rogach" %% "scallop" % scallopVersion,
  "io.circe" %% "circe-yaml" % circeYamlVersion,
  "org.scalatest" %% "scalatest-funspec" % scalaTestVersion % "test",
  "org.yaml" % "snakeyaml" % snaymlVersion
)
