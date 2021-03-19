
ThisBuild / organization := "org.alberto-perez-1994"
ThisBuild / version      := "1.0.0"

lazy val settings = Seq(

  scalaVersion := "2.13.4",

  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
)


lazy val core = project
  .in(file("core"))
  .settings(
    name := "core",
    settings
  )

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "macros",
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
