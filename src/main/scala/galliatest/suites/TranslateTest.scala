package galliatest.suites

import gallia._

// ===========================================================================
object TranslateTest extends gallia.testing.Suite {

  // ===========================================================================
  override def test() { import TestDataO.{Default01, Default01c, Default03, Default04, Default06, Default10, Default11}	  
    test1(in = Default01, out = Default01c)
    test2(in = Default06)
    test3(in = Default10)
    
    // ---------------------------------------------------------------------------
    Default10.translate(_.soleKey).usingLenient("foo" -> "oof").metaError(vldt.ErrorId.MoreThanOneKey)
    Default11.translate(_.soleKey).usingLenient("foo" -> "oof").check(bobj('f -> "oof"))
    Default11.translate(_.soleKey).usingStrict ("foo" -> 3    ).check(bobj('f -> 3))

    Default03.translate('p |> 'f ~> 'F).using("foo" -> "oof").check(bobj('p -> bobj('F -> "oof", 'g -> 1), 'z -> true))    
    Default03.translate('p |> 'f).using("foo" -> "oof").check(bobj('p ->     TestDataO.Default01c,                        'z -> true))
    Default04.translate('p |> 'f).using("foo" -> "oof").check(bobj('p -> Seq(TestDataO.Default01c, TestDataO.Default01b), 'z -> true))
  //Default03.add('p |> 'h -> true).check(bobj('f -> "foo", 'g -> 1, 'h -> true, 'h2 -> 3))
//FIXME : add nested, add objct + with cc    
  }
  
  // ===========================================================================
  private def test1(in: BObj, out: BObj) {
    in.translate(_.firstKey)   .using("foo" -> "oof").check(out)    
    in.translate(_.allBut(1))  .using("foo" -> "oof").check(out)
    in.translate(_.allButLast ).using("foo" -> "oof").check(out)
    in.translate(_.allButFirst).using(1 -> -1).check(bobj('f -> "foo", 'g -> -1))

    in.translate(_.allBut('g) ).using("foo" -> "oof").check(out)

    in.translate(           'f ~> 'F ).using("foo" -> "oof").check(bobj('F -> "oof", 'g -> 1))
  //in.translate(_.explicit('f ~> 'F)).using("foo" -> "oof").check(bobj('F -> "oof", 'g -> 1)) // TODO: re-allow?
    in.translate(_.explicit('f      )).using("foo" -> "oof").check(bobj('f -> "oof", 'g -> 1))

    in.translate('f).usingLenient("foo" -> "oof").check(out)
    in.translate('f).usingStrict ("foo" -> 3    ).check(bobj('f -> 3, 'g -> 1))
    in.translate('f).using("foo" -> "oof").check(bobj('f -> "oof", 'g -> 1))
  }
    
  // ===========================================================================
  private def test2(in: BObj) {  
    in.translate(_.indices(0,  1)).using("foo" -> "oof").check(bobj('f1 -> "oof", 'f2 -> "oof", 'g -> 1))
    in.translate(_.indices(0, -2)).using("foo" -> "oof").check(bobj('f1 -> "oof", 'f2 -> "oof", 'g -> 1))
    in.translate(_.indices(0,  5)).using("foo" -> "oof").metaError(vldt.ErrorId.OutOfBoundKey)
    in.translate(_.indices(0, -5)).using("foo" -> "oof").metaError(vldt.ErrorId.OutOfBoundKey)
  }
    
  // ===========================================================================
  private def test3(in: BObj) {
    in.translate(_.ifString           ).usingLenient("" -> ".", "@@@" -> "@").check(bobj('f -> ".", 'g -> 1, 'h -> ".", 'p -> bobj('pf ->  "", 'pg -> 2)))
    in.translate(_.ifStringRecursively).usingLenient("" -> ".", "@@@" -> "@").check(bobj('f -> ".", 'g -> 1, 'h -> ".", 'p -> bobj('pf -> ".", 'pg -> 2)))
    in.translate(_.soleKey).usingLenient("" -> 0).metaError(vldt.ErrorId.MoreThanOneKey)
    
    in.translate(_.ifString           ).usingLenient("" -> 0).metaError("201105140603", "MustBeSameType")
    in.translate(_.ifString           ).usingStrict ("" -> 0).check(bobj('f -> 0, 'g -> 1, 'h -> 0, 'p -> bobj('pf -> "", 'pg -> 2)))
    in.translate(_.ifStringRecursively).usingStrict ("" -> 0).check(bobj('f -> 0, 'g -> 1, 'h -> 0, 'p -> bobj('pf ->  0, 'pg -> 2)))
  //in.applyRecursivelyIfValue[String](_.startsWith("f")).using(_.toUpperCase)    
  }

}

// ===========================================================================
