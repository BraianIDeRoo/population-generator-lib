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

scalaVersion := "2.13.1"

val zioVersion = "1.0.0-RC18-2"
libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % "test",
  "dev.zio" %% "zio-test-sbt" % zioVersion % "test",
  "com.github.BraianIDeRoo" % "random-util_2.13" % "0.3.1"
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
