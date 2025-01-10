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
//  resolvers += "jitpack" at "https://jitpack.io",
  resolvers += "Artifactory" at "https://flow.jfrog.io/flow/libs-release/",
)

lazy val root = project
  .in(file("."))
  .settings(resolversSettings)
  .settings(
    testOptions += Tests.Argument("-oF"),
    libraryDependencies ++= Seq(
      "io.apibuilder" %% "apibuilder-commons" % "0.1.3",
      "com.typesafe.play" %% "play-json" % (if (scalaVersion.value.startsWith("3")) "2.10.6" else "2.9.4"),
      "com.typesafe.play" %% "play-json-joda" % (if (scalaVersion.value.startsWith("3")) "2.10.6" else "2.9.4"),
      "org.apache.commons" % "commons-compress" % "1.26.2",
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    ),
    scalacOptions ++= allScalacOptions,
    crossScalaVersions := supportedScalaVersions,
  )

publishTo := {
  val host = "https://flow.jfrog.io/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}

version := "0.5.2"
