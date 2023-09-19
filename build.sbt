// gallia-testing

// ===========================================================================
lazy val root = (project in file("."))
  .settings(
    organizationName     := "Gallia Project",
    organization         := "io.github.galliaproject", // *must* match groupId for sonatype
    name                 := "gallia-testing",
    version              := GalliaCommonSettings.CurrentGalliaVersion,
    homepage             := Some(url("https://github.com/galliaproject/gallia-testing")),
    scmInfo              := Some(ScmInfo(
        browseUrl  = url("https://github.com/galliaproject/gallia-testing"),
        connection =     "scm:git@github.com:galliaproject/gallia-testing.git")),
    licenses             := Seq("Apache 2" -> url("https://github.com/galliaproject/gallia-testing/blob/master/LICENSE")),
    description          := "A Scala library for data manipulation" )
  .settings(GalliaCommonSettings.mainSettings:_*)
  .dependsOn(RootProject(file("../gallia-core")) % "test->test") // see below
//libraryDependencies += "io.github.galliaproject" %% "gallia-core" % GalliaCommonSettings.CurrentGalliaVersion

// ---------------------------------------------------------------------------
// TODO: t210114171154, use existing testing library (or implement sbt-testing interfaces?)
  // note: in the process of moving all tests to using utest (see gallia-core's test folder)

// ---------------------------------------------------------------------------
libraryDependencies += "org.yaml" % "snakeyaml" % "1.30"

// ===========================================================================

