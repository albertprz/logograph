ThisBuild / organization := "com.albertprz"
ThisBuild / version      := "1.0.0"
ThisBuild / Compile / compile / logLevel := Level.Warn
Global / excludeLintKeys += logLevel
Global / onChangedBuildSource := ReloadOnSourceChanges
Test / parallelExecution := false


val scala3Ver = "3.3.4"
val scala2Ver = "2.13.8"


lazy val commonSettings = Seq(

  scalaVersion := scala3Ver,

  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) => Seq("-Ytasty-reader")
    case _             => Seq("-new-syntax", "-indent")
  }),

  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) => scalaReflect.map(_ % scalaVersion.value)
    case _             => Seq()
  })
)


lazy val coreSettings = Seq(
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => cats ++ catsEffect ++ pureconfig
      case _             => cats ++ catsEffect ++ pureconfig.map(_.cross(CrossVersion.for3Use2_13))
    })
)


lazy val testSettings = inConfig(IntegrationTest)(Defaults.itSettings) ++
  Seq (
    IntegrationTest / scalaSource := baseDirectory.value / "src/it/scala",
    TaskKey[Unit]("test") := (IntegrationTest / test).dependsOn(Test / test).value,
    libraryDependencies ++= (scalaTest ++ sqlite).map(_ % "it,test")
)



lazy val core = project
  .in(file("core"))
  .settings(
    name := "logograph-core",
    commonSettings,
    coreSettings
  )

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "logograph-macros",
    commonSettings,
    crossScalaVersions := Seq(scala3Ver, scala2Ver)
  )
.dependsOn(core)

lazy val app = project
  .in(file("."))
  .configs(IntegrationTest)
  .settings(
    name := "logograph",
    commonSettings,
    testSettings,
    crossScalaVersions := Seq(scala3Ver, scala2Ver)
  )
.dependsOn(core, macros)
.aggregate(core, macros)




/// Main Dependencies ///
val scalaReflect = Seq("org.scala-lang" % "scala-reflect")

val cats = Seq("org.typelevel" %% "cats-core"   % "2.6.1",
               "org.typelevel" %% "cats-kernel" % "2.6.1")

val catsEffect = Seq("org.typelevel" %% "cats-effect" % "3.1.1")

val pureconfig = Seq("com.github.pureconfig" %% "pureconfig" % "0.16.0")


/// Test Dependencies ///
val scalaTest = Seq("org.scalatest" %% "scalatest" % "3.2.9",
                    "org.scalactic" %% "scalactic" % "3.2.9")

val sqlite = Seq("org.xerial" % "sqlite-jdbc" % "3.47.1.0")
