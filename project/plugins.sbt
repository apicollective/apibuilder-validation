// Comment to get more information during initialization
logLevel := Level.Warn

// Artifactory credentials
credentials += Credentials(Path.userHome / ".ivy2" / ".artifactory")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.10")
