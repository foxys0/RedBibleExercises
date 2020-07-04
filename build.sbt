Global / name := "RedBibleExercises"
Global / organization := "cz.matejcerny"
Global / scalaVersion := "2.13.2"

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.1.2" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.1" % Test
    )
  )

// run just 10 tests instead of 100
Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-s", "10")