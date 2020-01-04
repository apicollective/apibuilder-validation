name := "apibuilder-validation"

organization := "io.flow"

scalaVersion in ThisBuild := "2.13.1"

crossScalaVersions := Seq("2.12.10", "2.13.1")

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.7.4",
      "org.apache.commons" % "commons-compress" % "1.18",
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.4" cross CrossVersion.full),
      "com.github.ghik" %% "silencer-lib" % "1.4.4" % Provided cross CrossVersion.full,
    ),
    credentials += Credentials(
      "Artifactory Realm",
      "flow.jfrog.io",
      System.getenv("ARTIFACTORY_USERNAME"),
      System.getenv("ARTIFACTORY_PASSWORD")
    ),
    // silence all warnings on autogenerated files
    scalacOptions += "-P:silencer:pathFilters=src/main/scala/io/apibuilder/generated/.*;src/test/scala/io/apibuilder/generated/.*"
  )

publishTo := {
  val host = "https://flow.jfrog.io/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}
version := "0.4.15"
