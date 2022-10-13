package galliatest
package suites.multiple

import gallia._
import aptus._

// ===========================================================================
object StreamerTest /* excluding RDDStreamer, tested in SparkTest */ extends gallia.testing.Suite {

  // ---------------------------------------------------------------------------
  override def test() {

    TestDataS.Default52._toBased(inMemory = true).rename("f" ~> "F").check(
      bobjs(
        bobj("F" -> "foo" , "g" -> 1),
        bobj("F" -> "foo" , "g" -> 1),
        bobj("F" -> "foo2", "g" -> 2)))

    TestDataS.Default52._toBased(inMemory = false).rename("f" ~> "F").check(
      bobjs(
        bobj("F" -> "foo" , "g" -> 1),
        bobj("F" -> "foo" , "g" -> 1),
        bobj("F" -> "foo2", "g" -> 2)))

    // ===========================================================================
    // Iterator Parallelism test
    {
      val in      : AObjs = Seq.fill[HeadZ](1000)(TestDataS.Default52).reduceLeft(_ union _)._forceResult
      val expected: AObjs =                       TestDataS.Default51.toUpperCase("f")      ._forceResult

      gallia.Hacks.iteratorParGroupSize.getValueOpt.assert(_.isEmpty)

      gallia.Hacks.iteratorParGroupSize.usingValue(3) { // if 4 cpus, then 4 * 3 = 12 elements per group
        in.toIteratorBased.toUpperCase("f").distinct.check(expected) }

      gallia.Hacks.iteratorParGroupSize.getValueOpt.assert(_.isEmpty)
    }
  }

}

// ===========================================================================