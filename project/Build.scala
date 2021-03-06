import sbt._
import Keys._

object MyBuild extends Build {
  lazy val commonSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.5",
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      "oss sonatype" at "https://oss.sonatype.org/content/repositories/releases",
      "spray repo" at "http://repo.spray.io"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.2" % "test",
      "io.spray" %% "spray-testkit" % "1.3.1" % "test",
      "com.typesafe.akka" %% "akka-actor" % "2.3.6",
      "io.spray" %% "spray-can" % "1.3.1",
      "io.spray" %% "spray-routing" % "1.3.1",
      "io.spray" %% "spray-http" % "1.3.1",
      "io.spray" %% "spray-json" % "1.2.6"
    ),
    scalacOptions ++= Seq(
      //"-Ymacro-debug-lite",
      "-deprecation"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  )

  lazy val root = Project(
    "root",
    file("."),
    settings = commonSettings ++ Seq(
      run <<= run in Compile in core
    )
  ) aggregate (macros, core)

  lazy val macros = Project(
    "macros",
    file("macros"),
    settings = commonSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies += "org.scalamacros" %% "resetallattrs" % "1.0.0-M1",
      scalacOptions ++= Seq(
        "-language:experimental.macros"
      )
    )
  )

  lazy val core = Project(
    "core",
    file("core"),
    settings = commonSettings
  ) dependsOn (macros)
}
