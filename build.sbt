

lazy val settings = Seq(

  scalaVersion := "2.13.4",

  libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai" %% "shapeless" % "2.3.3",
      "com.lihaoyi" % "ammonite" % "2.3.8-4-88785969" % "test" cross CrossVersion.full,
      "org.xerial" % "sqlite-jdbc" % "3.14.2"
  ),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),

  // Ammmonite REPL when invoking test:run in the REPL
  sourceGenerators in Test += Def.task {
    val file = (sourceManaged in Test).value / "amm.scala"
    IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
    Seq(file)
  }.taskValue
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

lazy val example = project
  .in(file("example"))
  .settings(
    name := "example",
    settings
  )
.dependsOn(app)
