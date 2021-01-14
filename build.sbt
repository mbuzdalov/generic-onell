lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  libraryDependencies ++= Seq(scalaTest, jacksonCore),
  scalaVersion := "2.13.4",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.2.3" % Test
lazy val jacksonCore = "com.fasterxml.jackson.core" % "jackson-core" % "2.12.1"

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "generic-onell", version := "0.0.0")
