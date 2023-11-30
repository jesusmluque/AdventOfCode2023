ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2023"
  )

libraryDependencies += ("org.scalatest" %% "scalatest" % "3.2.10") // .withDottyCompat(scalaVersion.value)
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"