name := "fantasy-population-generator"

inThisBuild(
  List(
    organization := "com.github.BraianIDeRoo",
    homepage := Some(
      url("https://github.com/BraianIDeRoo/population-generator-lib")
    ),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "BraianIDeRoo",
        "Braian De Roo",
        "braianideroo@gmail.com",
        url("https://github.com/BraianIDeRoo")
      )
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/BraianIDeRoo/population-generator-lib"),
        "git@github.com:BraianIDeRoo/population-generator-lib.git"
      )
    )
  )
)

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

ThisBuild / scalaVersion := "2.13.2"

skip in publish := true

val zioVersion = "1.0.0-RC20"

val fantasyPopulationGenerator = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "fantasy-population-generator",
    version := "0.2.2",
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio" % zioVersion,
      "dev.zio" %%% "zio-test" % zioVersion % "test",
      "dev.zio" %%% "zio-test-sbt" % zioVersion % "test",
      "com.github.BraianIDeRoo" %%% "random-util" % "0.5.2"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.0.0"
  )

lazy val root = project
  .in(file("."))
  .aggregate(fantasyPopulationGenerator.js, fantasyPopulationGenerator.jvm)
  .settings(scalaVersion := "2.13.2", publish := {}, publishLocal := {})
