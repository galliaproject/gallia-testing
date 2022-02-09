package galliatest.suites

import gallia._

// ===========================================================================
object GraphTest extends gallia.testing.Suite {	

  override def test() {
    val in: HeadO = bobj('f -> "foo", 'g -> 1)

      // regression test for core's 0aa5d8d8 bug fix
      in.rename('g ~> 'g1).convertToMultiple
        .innerJoin {
      in.rename('g ~> 'g2).convertToMultiple }
        .check(bobjs(bobj('f -> "foo", 'g1 -> 1, 'g2 -> 1)))
  }

}

// ===========================================================================
