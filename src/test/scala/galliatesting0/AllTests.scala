package galliatesting0

// ===========================================================================
object AllTests extends App {
  import suites._

  // ---------------------------------------------------------------------------
  gallia.testing.More.testSuites(
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
    
    // single.DataClassesTest, -> moved to gallia-core (using utest)
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
    //TimeTest, -> moved to gallia-core (using utest)
    MetaSchemaTest,
  )
  if (false) Obg9Test // obg9 is codename for memory-optimized Obj counterpart (not in use yet) - good for dense data

}

// ===========================================================================
