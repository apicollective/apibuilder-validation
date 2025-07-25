lazy val scala2 = "2.13.15"
lazy val scala3 = "3.4.2"
lazy val supportedScalaVersions = List(scala2, scala3)

name := "apibuilder-validation"

organization := "io.apibuilder"

ThisBuild / scalaVersion := scala3

lazy val allScalacOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
  "-Wconf:src=generated/.*:silent",
  "-deprecation"
)


lazy val resolversSettings = Seq(
  resolvers += "jitpack" at "https://jitpack.io",
)

lazy val root = project
  .in(file("."))
  .settings(resolversSettings)
  .settings(
    testOptions += Tests.Argument("-oF"),
    libraryDependencies ++= Seq(
      "com.github.apicollective" % "apibuilder-commons" % (if (scalaVersion.value.startsWith("3")) "0.1.0" else "0.0.6"),
      "com.typesafe.play" %% "play-json" % (if (scalaVersion.value.startsWith("3")) "2.10.6" else "2.9.4"),
      "com.typesafe.play" %% "play-json-joda" % (if (scalaVersion.value.startsWith("3")) "2.10.6" else "2.9.4"),
      "org.apache.commons" % "commons-compress" % "1.26.2",
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    ),
    scalacOptions ++= allScalacOptions,
    crossScalaVersions := supportedScalaVersions,
  )


version := "0.5.4"
