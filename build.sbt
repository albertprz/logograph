
val sparkVersion = "2.4.5"

lazy val settings = Seq(

  scalaVersion := "2.13.2",
  name := "pipeline",

  libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
  libraryDependencies +=  "com.lihaoyi" % "ammonite" % "2.1.4" cross CrossVersion.full,
  // libraryDependencies +=  "sh.almond" %% "ammonite-spark" % "0.9.0",

  // libraryDependencies ++= Seq(
  //   "org.apache.spark" %% "spark-core" % sparkVersion,
  //   "org.apache.spark" %% "spark-sql" % sparkVersion,
  //   "org.apache.spark" %% "spark-streaming" % sparkVersion,
  // ),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),

  scalacOptions ++= Seq("-Ymacro-debug-lite"),

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



lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "macros",
    settings
  )

lazy val app = project
  .in(file("app"))
  .settings(
    name := "app",
    settings
  )
.dependsOn(macros)
