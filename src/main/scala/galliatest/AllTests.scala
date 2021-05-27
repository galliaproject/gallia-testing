package galliatest

// ===========================================================================
object AllTests extends App {
  import suites._

  // ---------------------------------------------------------------------------
  gallia.testing.testSuites(
    single.VeryBasicsTest,
    single.SomewhatBasicsTest,

    single.SetDefaultValueForTest,
    single.TranslateTest,
    single.RemoveIfTest,
    
    single.AssertTest,

    single.TransformTest,
    single.FuseFissionTest,
    single.GenerateTest,
    
    single.UntuplifyTest,
    single.ZipTest,
    
    // ---------------------------------------------------------------------------
    multiple.FilterTest,
    multiple.SortingTest,
    multiple.GroupingTest, 
    multiple.AggregatingTest,
    multiple.ReducingTest,
    multiple.StatsTest,
    multiple.PivotingTest,
    multiple.MergingTest)

}

// ===========================================================================
