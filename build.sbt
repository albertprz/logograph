

lazy val settings = Seq(

  scalaVersion := "2.13.4",

  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
)


lazy val common = project
  .in(file("common"))
  .settings(
    name := "common",
    settings
  )

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "macros",
    settings
  )
.dependsOn(common)

lazy val app = project
  .in(file("app"))
  .settings(
    name := "app",
    settings
  )
.dependsOn(common, macros)
