Global / name := "RedBibleExercises"
Global / organization := "cz.matejcerny"
Global / scalaVersion := "2.13.2"

lazy val RedBibleExercises = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.1.2" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.1" % Test
    )
  )

Global / scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-language:higherKinds", // Allow higher-kinded types
  "-Ywarn-unused:imports" // Warn if an import selector is not referenced.
)