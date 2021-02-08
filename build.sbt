

lazy val settings = Seq(

  scalaVersion := "2.13.4",


  libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai" %% "shapeless" % "2.3.3",
      "com.lihaoyi" % "ammonite" % "2.3.8-4-88785969" % "test" cross CrossVersion.full
  ),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),

  // scalacOptions ++= Seq("-Ymacro-debug-lite"),


  // Ammmonite REPL when invoking test:run in the REPL
  sourceGenerators in Test += Def.task {
    val file = (sourceManaged in Test).value / "amm.scala"
    IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
    Seq(file)
  }.taskValue,

  // Optional, required for the `source` command to work
  (fullClasspath in Test) ++= {
    (updateClassifiers in Test).value
      .configurations
      .find(_.configuration.name == Test.name)
      .get
      .modules
      .flatMap(_.artifacts)
      .collect{ case (a, f) if a.classifier == Some("sources") => f }
  }

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
.dependsOn(common)
.dependsOn(macros)
