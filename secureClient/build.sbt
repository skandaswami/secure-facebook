name := "fbclient"

version := "1.0"

scalaVersion := "2.11.7"

scalaVersion := "2.11.7"

libraryDependencies ++=
  {
    val akkaVersion = "2.4.0"
    val sprayVersion = "1.3.3"
    Seq (
      "io.spray" %% "spray-can" % sprayVersion,
      "io.spray" %% "spray-client" % sprayVersion,
      "io.spray" %% "spray-json" % "1.3.2",
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-remote" % akkaVersion,
      "commons-codec" % "commons-codec" % "1.10"
    )
  }