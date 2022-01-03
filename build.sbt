// gallia-testing

// ===========================================================================
lazy val root = (project in file("."))
  .settings(
    organizationName     := "Gallia Project",
    organization         := "io.github.galliaproject", // *must* match groupId for sonatype
    name                 := "gallia-testing",
    version              := "0.3.0",    
    homepage             := Some(url("https://github.com/galliaproject/gallia-testing")),
    scmInfo              := Some(ScmInfo(
        browseUrl  = url("https://github.com/galliaproject/gallia-testing"),
        connection =     "scm:git@github.com:galliaproject/gallia-testing.git")),
    licenses             := Seq("BSL 1.1" -> url("https://github.com/galliaproject/gallia-testing/blob/master/LICENSE")),
    description          := "A Scala library for data manipulation" )
  .settings(GalliaCommonSettings.mainSettings:_*)

// ===========================================================================    
lazy val galliaVersion = "0.3.2"

// ---------------------------------------------------------------------------
libraryDependencies += "io.github.galliaproject" %% "gallia-core" % galliaVersion

// ---------------------------------------------------------------------------
// TODO: t210114171154, use existing testing library (or implement sbt-testing interfaces?)

// ===========================================================================
sonatypeRepository     := "https://s01.oss.sonatype.org/service/local"
sonatypeCredentialHost :=         "s01.oss.sonatype.org"        
publishMavenStyle      := true
publishTo              := sonatypePublishToBundle.value

// ===========================================================================

