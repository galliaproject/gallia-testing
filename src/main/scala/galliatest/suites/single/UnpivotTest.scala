package galliatest.suites.single

import gallia._

// ===========================================================================
object UnpivotTest extends gallia.testing.Suite {

  override def test() {
    val in  = bobj('f -> "foo", 'g -> 1,                           "a" ->       "A",              "b" ->       "B",              "c" ->       "C")
    val out = bobj('f -> "foo", 'g -> 1, _group -> Seq(bobj(_id -> "a", _vle -> "A"), bobj(_id -> "b", _vle -> "B"), bobj(_id -> "c", _vle -> "C")))
    in.unpivot("a", "b", "c").check(out)
  }

}

// ===========================================================================
