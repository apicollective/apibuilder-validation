name := "apibuilder-validation"

organization := "io.apibuilder"

ThisBuild / scalaVersion := "3.4.2"

lazy val allScalacOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
)

lazy val resolversSettings = Seq(
  resolvers += "jitpack" at "https://jitpack.io"
)

lazy val root = project
  .in(file("."))
  .settings(resolversSettings)
  .settings(
    testOptions += Tests.Argument("-oF"),
    libraryDependencies ++= Seq(
      "com.github.apicollective" % "apibuilder-commons" % "0.1.0",
      "com.typesafe.play" %% "play-json" % "2.10.6",
      "com.typesafe.play" %% "play-json-joda" % "2.10.6",
      "org.apache.commons" % "commons-compress" % "1.26.2",
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    ),
    scalacOptions ++= allScalacOptions,
  )
version := "0.5.1"
