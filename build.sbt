val scala3Version = "3.2.1"

Global / cancelable := true
Global / fork := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "eightqueen",
    version := "0.1.0-SNAPSHOT",
    organization := "io.github.yzia2000",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined" % "0.10.1",
      "dev.zio" %% "zio" % "2.0.5",
      "dev.zio" %% "zio-streams" % "2.0.5",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
