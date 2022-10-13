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
    licenses             := Seq("BSL 1.1" -> url("https://github.com/galliaproject/gallia-testing/blob/master/LICENSE")),
    description          := "A Scala library for data manipulation" )
  .settings(GalliaCommonSettings.mainSettings:_*)

// ===========================================================================
libraryDependencies += "io.github.galliaproject" %% "gallia-core" % GalliaCommonSettings.CurrentGalliaVersion

// ---------------------------------------------------------------------------
// TODO: t210114171154, use existing testing library (or implement sbt-testing interfaces?)
libraryDependencies += "org.yaml" % "snakeyaml" % "1.30"

// ===========================================================================
sonatypeRepository     := "https://s01.oss.sonatype.org/service/local"
sonatypeCredentialHost :=         "s01.oss.sonatype.org"
publishMavenStyle      := true
publishTo              := sonatypePublishToBundle.value

// ===========================================================================

