package galliatest.suites

import gallia._

// ===========================================================================
object IoTest extends gallia.testing.Suite {	

  override def test() {   
    // TODO: port rest from old tests
    
    // ---------------------------------------------------------------------------
    Seq(("foo", 1), ("bar", 2))
      .toHead { case (f, g) => bobj("f" -> f, "g" -> g) }
        .check(bobjs(bobj('f -> "foo", 'g -> 1), bobj('f -> "bar", 'g -> 2)))    
  }

}

// ===========================================================================
