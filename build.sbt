name := "apibuilder-validation"

organization := "io.apibuilder"

scalaVersion in ThisBuild := "2.12.8"

crossScalaVersions := Seq("2.12.8")

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.7.1",
      "org.apache.commons" % "commons-compress" % "1.18",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    ),
    credentials += Credentials(
      "Artifactory Realm",
      "flow.jfrog.io",
      System.getenv("ARTIFACTORY_USERNAME"),
      System.getenv("ARTIFACTORY_PASSWORD")
    )
  )

publishTo := {
  val host = "https://flow.jfrog.io/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}
version := "0.3.22"
