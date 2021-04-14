package galliatest

// ===========================================================================
object AllTests extends App {
  import suites._

  // ---------------------------------------------------------------------------
  gallia.testing.testSuites(
    VeryBasicsTest,
    SomewhatBasicsTest,

    SetDefaultValueForTest,
    TranslateTest,
    
    AssertTest,

    PivotingTest,
    MergingTest)

}

// ===========================================================================
