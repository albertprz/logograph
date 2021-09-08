ThisBuild / organization := "com.albertoperez1994"
ThisBuild / version      := "1.0.0"
ThisBuild / scalaVersion := scalaVer

ThisBuild / Compile / compile / logLevel := Level.Warn
Global / excludeLintKeys += logLevel
Global / onChangedBuildSource := ReloadOnSourceChanges
Test / parallelExecution := false


val scalaVer = "2.13.5"

lazy val core = project
  .in(file("core"))
  .settings(
    name := "scalaql-core",
    libraryDependencies ++= mainDependencies
  )

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "scalaql-macros",
    libraryDependencies ++= mainDependencies
  )
.dependsOn(core)

lazy val app = project
  .in(file("."))
  .configs(IntegrationTest)
  .settings(
    name := "scalaql",
    itSettings,
    libraryDependencies ++= mainDependencies ++ testDependencies.map(_ % "it,test")
  )
.dependsOn(core, macros)
.aggregate(core, macros)


lazy val mainDependencies = scalaReflect ++ cats ++ catsEffect ++ circe ++ pureconfig

lazy val testDependencies = scalaTest ++ scalaCheck ++ sqlite

lazy val itSettings = inConfig(IntegrationTest)(Defaults.itSettings) ++
                        Seq (IntegrationTest / scalaSource := baseDirectory.value / "src/it/scala",
                             TaskKey[Unit]("test") := (IntegrationTest / test).dependsOn(Test / test).value)

/// Dependencies ///
val scalaReflect = Seq("org.scala-lang" % "scala-reflect" % scalaVer)

val cats = Seq("org.typelevel" %% "cats-core"   % "2.6.0",
               "org.typelevel" %% "cats-kernel" % "2.6.0")

val catsEffect = Seq("org.typelevel" %% "cats-effect" % "3.1.1")

val circe = Seq("io.circe" %% "circe-core"    % "0.14.1",
                "io.circe" %% "circe-generic" % "0.14.1",
                "io.circe" %% "circe-parser"  % "0.14.1")

val pureconfig = Seq("com.github.pureconfig" %% "pureconfig" % "0.16.0")

val scalaTest = Seq("org.scalatest" %% "scalatest" % "3.2.9",
                    "org.scalactic" %% "scalactic" % "3.2.9")

val scalaCheck = Seq("org.scalacheck" %% "scalacheck" % "1.14.1")

val sqlite = Seq("org.xerial" % "sqlite-jdbc" % "3.14.2")
