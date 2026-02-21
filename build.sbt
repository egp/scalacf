// build.sbt v2
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.8.1"
ThisBuild / organization := "net.egp"

lazy val root = (project in file("."))
  .settings(
    name := "scalacf",

    libraryDependencies +=
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,

    coverageEnabled := true,
    coverageMinimumStmtTotal := 90,
    coverageFailOnMinimum := false
  )

  Test / javaOptions ++= Seq(
  "-Xmx4G",
  "-XX:+UseG1GC"
)

// EOF
