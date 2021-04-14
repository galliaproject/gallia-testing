package galliatest

// ===========================================================================
object AllTests extends App {
  import suites._

  // ---------------------------------------------------------------------------
  gallia.testing.testSuites(
    VeryBasicsTest,
    SomewhatBasicsTest,

    AssertTest,

    PivotingTest,
    MergingTest)

}

// ===========================================================================
