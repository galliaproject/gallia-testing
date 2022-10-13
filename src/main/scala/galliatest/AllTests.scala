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

    single.CommonTransformationsTest,
    
    single.NestingRelatedTest,
    single.TransformTest,
    single.FissionTest,
    single.FuseTest,
    single.GenerateTest,
    single.CotransformTest,
    
    single.DeserializeTest,
    single.ZipTest,
    
    single.ForXTest,
    
    single.DataClassesTest,
    single.SquashingTest,
    single.WhateverTest,
    single.UncommonTypesTest,

    single.UnpivotTest,
    
    // ---------------------------------------------------------------------------
    multiple.StreamerTest,

    multiple.FilterByTest,
    multiple.FlattenByTest,
    multiple.SortingTest,
    multiple.GroupingTest, 
    multiple.AggregatingTest,
    multiple.ReducingTest,
    multiple.StatsTest,
    multiple.PivotingTest,
    multiple.MergingTest,
    
    multiple.HeadZTest,
    
    // ---------------------------------------------------------------------------
    CustomTest,
    GraphTest,
    IoTest,
    SchemaInferrerTest,
    TargetSelectionTest,
    RuntimeValidationTest,
    UnionTypeTest,
    EnumTest,
    TimeTest,
    MetaSchemaTest,
  )
  if (false) Obg9Test // obg9 is codename for memory-optimized Obj counterpart (not in use yet) - good for dense data

}

// ===========================================================================
