val circeVersion = "0.6.1"
val akkaVersion = "10.0.1"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

lazy val root = (project in file(".")).
  settings(
    name := "generalsio",
    version := "0.0.1",
    sbtVersion := "0.13.13",
    scalaVersion := "2.12.1",

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-optics" % circeVersion,
      "com.typesafe.akka" %% "akka-http-core" % akkaVersion,
      "org.atnos" %% "eff" % "2.2.0",
      "org.spire-math" %% "spire" % "0.13.0",

      "org.specs2" %% "specs2-core" % "3.8.6" % "test"
    ),

    scalacOptions ++= Seq("-Xfatal-warnings", "-Xlint", "-Ywarn-unused-import", "-Ywarn-dead-code",
                          "-Ywarn-dead-code", "-Ywarn-numeric-widen", "-Ypartial-unification"),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )
