// gallia-testing; TODO: t210309100048 - relies on symlink to gallia-core's project/*.scala files; no (reasonnable) sbt way? windows users will have to copy them instead?

// ===========================================================================
lazy val root = (project in file("."))
  .settings(
    name    := "gallia-testing",
    version := "0.1.0")
  .settings(GalliaCommonSettings.mainSettings:_*)
  .dependsOn(RootProject(file("../gallia-core")))

// ===========================================================================
// TODO: t210114171154, use existing testing library (or implement sbt-testing interfaces?)

// ===========================================================================

