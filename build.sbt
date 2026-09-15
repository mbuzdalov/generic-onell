lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  libraryDependencies ++= Seq(scalaTest, jGraphT),
  scalaVersion := "3.9.0",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.2.20" % Test
lazy val jGraphT = "org.jgrapht" % "jgrapht-core" % "1.5.3" // used for solving vertex covers

lazy val root = project
  .in(file("."))
  .settings(commonSettings*)
  .settings(name := "generic-onell", version := "0.0.0")
