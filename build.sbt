name := "apibuilder-validation"

organization := "io.apibuilder"

ThisBuild / scalaVersion := "2.13.6"

crossScalaVersions := Seq("2.13.6")

lazy val allScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Ypatmat-exhaust-depth", "100", // Fixes: Exhaustivity analysis reached max recursion depth, not all missing cases are reported.
  "-Wconf:src=generated/.*:silent",
  "-Wconf:src=target/.*:silent", // silence the unused imports errors generated by the Play Routes
)

lazy val resolversSettings = Seq(
  resolvers += "Artifactory" at "https://flow.jfrog.io/flow/libs-release/",
  resolvers += "Flow Artifactory" at "https://flow.jfrog.io/flow/libs-release-local/",
  credentials += Credentials(
    "Artifactory Realm",
    "flow.jfrog.io",
    System.getenv("ARTIFACTORY_USERNAME"),
    System.getenv("ARTIFACTORY_PASSWORD")
  )
)

lazy val root = project
  .in(file("."))
  .settings(resolversSettings)
  .settings(
    testOptions += Tests.Argument("-oF"),
    libraryDependencies ++= Seq(
      "io.apibuilder" %% "apibuilder-commons" % "0.0.6",
      "com.typesafe.play" %% "play-json" % "2.9.0",
      "com.typesafe.play" %% "play-json-joda" % "2.9.0",
      "org.apache.commons" % "commons-compress" % "1.20",
      "org.typelevel" %% "cats-core" % "2.1.1",
      "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    ),
    credentials += Credentials(
      "Artifactory Realm",
      "flow.jfrog.io",
      System.getenv("ARTIFACTORY_USERNAME"),
      System.getenv("ARTIFACTORY_PASSWORD")
    ),
    scalacOptions ++= allScalacOptions,
  )

publishTo := {
  val host = "https://flow.jfrog.io/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}
version := "0.4.31"
