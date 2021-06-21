package galliatest.suites.multiple

import gallia._

// ===========================================================================
object FlattenByTest extends gallia.testing.Suite {
	import TestDataS._

	// ---------------------------------------------------------------------------
  override def test() {

    bobj('f -> "foo", 'g -> Seq(1, 2, 3)).flattenBy('g).check(
      bobjs(bobj('f -> "foo", 'g -> 1), bobj('f -> "foo", 'g -> 2), bobj('f -> "foo", 'g -> 3)) )

    Default59.flattenBy('f).check(aobjs(cls('f.string_, 'g.int))(
        obj('f -> "foo1", 'g -> 1),
        obj('f -> "foo2", 'g -> 1),
        obj(              'g -> 1),
        obj('f -> "bar1", 'g -> 2),
        obj('f -> "bar2", 'g -> 2),
        obj('f -> "foo1", 'g -> 1),
        obj('f -> "foo2", 'g -> 1)))

    Default60.flattenBy('f).check(bobjs(
        bobj('f -> "foo1", 'g -> 1),
        bobj('f -> "foo2", 'g -> 1),
        bobj('f -> "foo3", 'g -> 3),
        bobj('f -> "foo4", 'g -> 3)))

    // ---------------------------------------------------------------------------
    // uz - also see flattenBy

    bobj('f1 -> "foo1", 'g1 -> 1).convertToZ.check(
        bobjs(bobj('f1 -> "foo1", 'g1 -> 1)))
  }
}

// ===========================================================================
