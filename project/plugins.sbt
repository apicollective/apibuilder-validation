// Comment to get more information during initialization
logLevel := Level.Warn

// Artifactory credentials
credentials += Credentials(Path.userHome / ".ivy2" / ".artifactory")

resolvers += "Artifactory" at "https://flow.artifactoryonline.com/flow/libs-release-local/"

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.4")
