lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  libraryDependencies ++= Seq(scalaTest, jacksonCore, jGraphT),
  scalaVersion := "2.13.13",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.2.18" % Test
lazy val jacksonCore = "com.fasterxml.jackson.core" % "jackson-core" % "2.17.0"
lazy val jGraphT = "org.jgrapht" % "jgrapht-core" % "1.5.2" // used for solving vertex covers

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "generic-onell", version := "0.0.0")
