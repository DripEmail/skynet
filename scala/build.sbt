lazy val root = (project in file(".")).
  settings(
    name := "skynet",
    organization := "com.temerev",
    scalaVersion := "2.11.7",
    version := "0.1.0",
//    sbtVersion := "0.13.9",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.5.4"
    )
  )

cancelable in Global := true

fork := true

