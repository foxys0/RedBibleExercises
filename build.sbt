Global / name := "RedBibleExercises"
Global / organization := "cz.matejcerny"
Global / scalaVersion := "2.13.2"

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % Test
  )
