ThisBuild / organization := "com.albertoperez1994"
ThisBuild / version      := "1.0.0"
ThisBuild / Compile / compile / logLevel := Level.Warn
Global / excludeLintKeys += logLevel
Global / onChangedBuildSource := ReloadOnSourceChanges
Test / parallelExecution := false


lazy val mainSettings = Seq(

  scalaVersion := "2.13.6",

  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) => Seq("-Ytasty-reader")
    case _             => Seq()
  }),

  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) => cats ++ catsEffect ++ pureconfig ++ scalaReflect.map(_ % scalaVersion.value)
    case _             => cats ++ catsEffect ++ pureconfig.map(_.cross(CrossVersion.for3Use2_13))
  })
)


lazy val core = project
  .in(file("core"))
  .settings(
    name := "scalaql-core",
    mainSettings,
    scalaVersion := "3.0.0",
  )

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "scalaql-macros",
    mainSettings,
  )
.dependsOn(core)

lazy val app = project
  .in(file("."))
  .configs(IntegrationTest)
  .settings(
    name := "scalaql",
    mainSettings,
    integrationTestSettings,
    libraryDependencies ++= testDependencies,
  )
.dependsOn(core, macros)
.aggregate(core, macros)



lazy val testDependencies = (scalaTest ++ sqlite).map(_ % "it,test")

lazy val integrationTestSettings = inConfig(IntegrationTest)(Defaults.itSettings) ++
                                      Seq (IntegrationTest / scalaSource := baseDirectory.value / "src/it/scala",
                                           TaskKey[Unit]("test") := (IntegrationTest / test).dependsOn(Test / test).value)


/// Dependencies ///
val scalaReflect = Seq("org.scala-lang" % "scala-reflect")

val cats = Seq("org.typelevel" %% "cats-core"   % "2.6.1",
               "org.typelevel" %% "cats-kernel" % "2.6.1")

val catsEffect = Seq("org.typelevel" %% "cats-effect" % "3.1.1")

val pureconfig = Seq("com.github.pureconfig" %% "pureconfig" % "0.16.0")

val scalaTest = Seq("org.scalatest" %% "scalatest" % "3.2.9",
                    "org.scalactic" %% "scalactic" % "3.2.9")

val sqlite = Seq("org.xerial" % "sqlite-jdbc" % "3.14.2")
