lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  libraryDependencies += scalaTest,
  scalaVersion := "2.13.0",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.1.0" % Test

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "generic-onell", version := "0.0.0")
