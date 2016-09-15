name := "lib-apidoc-json-validation"

organization := "io.flow"

scalaVersion in ThisBuild := "2.11.8"

version := "0.0.6"

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.5.8",
      "org.scalatest" %% "scalatest" % "2.2.6" % Test
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
