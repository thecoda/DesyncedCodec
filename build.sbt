val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "DesyncedCodec",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scodec" %% "scodec-core" % "2.2.1",
      "com.lihaoyi" %% "pprint" % "0.8.1",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
