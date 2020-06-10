lazy val root = project
  .in(file("."))
  .settings(
    name := "RedBibleExercises",
    scalaVersion := "0.24.0-RC1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % Test
  )
