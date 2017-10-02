name := "apibuilder-validation"

organization := "io.flow"

scalaVersion in ThisBuild := "2.12.3"

crossScalaVersions := Seq("2.12.3", "2.11.11")

version := "0.1.6"

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.6.6",
      "org.scalatest" %% "scalatest" % "3.0.4" % Test
    )
  )

publishTo := {
  val host = "https://flow.artifactoryonline.com/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}
