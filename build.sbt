Global / name := "RedBibleExercises"
Global / organization := "cz.matejcerny"
Global / scalaVersion := "2.13.3"

val CatsVersion = "2.2.0"
val ScalaTestVersion = "3.1.2"
val ScalaCheckVersion = "1.14.1"

lazy val RedBibleExercises = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
      "org.scalacheck" %% "scalacheck" % ScalaCheckVersion % Test
    ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
    )
  )

Global / scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-language:higherKinds", // Allow higher-kinded types
  "-Ywarn-unused:imports" // Warn if an import selector is not referenced.
)
