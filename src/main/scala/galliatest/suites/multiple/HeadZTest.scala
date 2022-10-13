package galliatest.suites.multiple

import gallia._

// ===========================================================================
object HeadZTest extends gallia.testing.Suite {
    import TestDataO._

  // ===========================================================================
  override def test() {
    if (false) bobjs(Default00, Default00b).mergeAll//.check
    bobjs(Default01, Default01b).retain('g).asArray1          .check(bobj('g -> Seq(1, 2)))
    bobjs(Default01, Default01b).retain('g).asArray1('g ~> 'G).check(bobj('G -> Seq(1, 2)))
    bobjs(Default01, Default01b)           .asArray2          .check(bobj(_array -> Seq(bobj('f -> "foo", 'g -> 1), bobj('f -> "foo2", 'g -> 2))))
    bobjs(Default01, Default01b)           .asArray2('ARRAY)  .check(bobj('ARRAY -> Seq(bobj('f -> "foo", 'g -> 1), bobj('f -> "foo2", 'g -> 2))))
  }
}

// ===========================================================================
