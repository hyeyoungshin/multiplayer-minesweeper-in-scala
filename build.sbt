val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Minesweeper",
    version := "0.0.1",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "0.7.29" % Test, 
                                "org.scala-lang" %% "toolkit" % "0.1.7",
                                "org.scalatest" %% "scalatest" % "3.2.18" % "test",
                                "com.lihaoyi" %% "upickle" % "3.1.0"
                                )
  )
