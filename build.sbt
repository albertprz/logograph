
ThisBuild / organization := "com.albertoperez1994"
ThisBuild / version      := "1.0.0"

lazy val settings = Seq(

  scalaVersion := "2.13.0",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
)


lazy val core = project
  .in(file("core"))
  .settings(
    name := "scalaql-core",
    settings
  )

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "scalaql-macros",
    settings
  )
.dependsOn(core)

lazy val app = project
  .in(file("."))
  .settings(
    name := "scalaql",
    settings
  )
.dependsOn(core, macros)
.aggregate(core, macros)
