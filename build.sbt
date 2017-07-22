lazy val akkaVersion = "2.5.3"
lazy val hello = taskKey[Unit]("An example task")
lazy val commonSettings = Seq(
    organization := "dk.itu.sdt",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.12.2")
lazy val root = (project in file("."))
    .settings(
        name := "conll",
        commonSettings,
        hello := {println("Hello")},
        libraryDependencies ++= Seq(
            "com.typesafe.akka" %% "akka-actor" % akkaVersion,
            "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
            "org.scalatest" %% "scalatest" % "3.0.1" % "test",
            "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
        )
    )
