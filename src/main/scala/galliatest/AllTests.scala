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
    single.ConvertTest,
    
    single.AssertTest,

    single.NestingRelatedTest,
    single.TransformTest,
    single.FissionTest,
    single.FuseTest,
    single.GenerateTest,
    single.CotransformTest,
    
    single.UntuplifyTest,
    single.ZipTest,
    
    single.ForXTest,
    
    single.CcMappingTest,
    single.SquashingTest,
    single.WhateverTest,
    single.UncommonTypesTest,
single.UnpivotTest,    
    // ---------------------------------------------------------------------------
    multiple.FilterTest,
    multiple.FlattenByTest,
    multiple.SortingTest,
    multiple.GroupingTest, 
    multiple.AggregatingTest,
    multiple.ReducingTest,
    multiple.StatsTest,
    multiple.PivotingTest,
    multiple.MergingTest,
    
    // ---------------------------------------------------------------------------
    SchemaInferrerTest,
    )

}

// ===========================================================================
