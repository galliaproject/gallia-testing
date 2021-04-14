package galliatest.suites

import gallia._

// ===========================================================================
object SomewhatBasicsTest extends gallia.testing.Suite {
  import gallia.vldt.ErrorId.{FieldAlreadyExists, MoreThanOneKey, OutOfBoundKey}
  import TestDataO.{
    Default01, Default01b, Default01c,
    Default03,
    Default04,
    Default06,
    Default10,
    Default11}

  // ===========================================================================
  def test() {
    testRenameDynamically()

    // ===========================================================================
    // split

    bobj('f -> "foo,bar,baz", 'g -> 1).split('f).by(",") check bobj('f -> Seq("foo", "bar", "baz"), 'g -> 1)

    // ===========================================================================
    testTranslate1(Default01)
    testTranslate2(Default10)
    testTranslate3()

  //Default03.add('p |> 'h -> true).test(bobj('f -> "foo", 'g -> 1, 'h -> true, 'h2 -> 3))
//FIXME: add nested, add objct + with cc
    // ===========================================================================
    testSwap()
    testCopy()

    // ===========================================================================
    testMiscStringOps(Default01)
  }

  // ===========================================================================
  private def testRenameDynamically() {
    Default01.rename { skey => if (skey == "g") "f" else skey } metaError FieldAlreadyExists

    Default01.rename           (_.toUpperCase) check            bobj('F -> "foo", 'G -> 1)
    Default03.renameRecursively(_.toUpperCase) check bobj('P -> bobj('F -> "foo", 'G -> 1), 'Z -> true)

    if (false) // TODO: catch error properly, right now it selected Seq(Symbol(""), Symbol(""))
      bobj('f -> "foo", 'gg -> 1).rename(_.tail.tail) metaError gallia.vldt.ErrorId.CouldNotRenameDynamically // "f - empty.tail"

    val in = bobj('f -> "foo", 'gg -> 1)
      in.noop(_.forEachKey(_.customKeys(_.tail.tail)).zen((u, k) => u.rename(k ~> k.name.toUpperCase)))
      in       .forEachKey(_.customKeys(_.tail     )).zen((u, k) => u.rename(k ~> k.name.toUpperCase)) check bobj('f -> "foo", 'GG -> 1)
  }

  // ===========================================================================
  private def testTranslate1(in: BObj) {
    in.translate(_.firstKey)   .using("foo" -> "oof") check Default01c
    in.translate(_.allBut(1))  .using("foo" -> "oof") check Default01c
    in.translate(_.allButLast) .using("foo" -> "oof") check Default01c
    in.translate(_.allButFirst).using(1 -> -1)        check bobj('f -> "foo", 'g -> -1)

    in.translate(_.allBut('g) ).using("foo" -> "oof") check Default01c

    in.translate(           'f ~> 'F ).using("foo" -> "oof") check bobj('F -> "oof", 'g -> 1)
    in.translate(_.explicit('f      )).using("foo" -> "oof") check bobj('f -> "oof", 'g -> 1)
  //in.translate(_.explicit('f ~> 'F)).using("foo" -> "oof") check bobj('F -> "oof", 'g -> 1) // TODO: re-allow?

    in.translate('f).usingLenient("foo" -> "oof") check Default01c
    in.translate('f).usingStrict ("foo" -> 3    ) check bobj('f -> 3, 'g -> 1)

    in.translate('f).using("foo" -> "oof") check bobj('f -> "oof", 'g -> 1)
  }

  // ---------------------------------------------------------------------------
  private def testTranslate2(in: BObj) {
    in.translate(_.ifString           ).usingLenient("" -> ".", "@@@" -> "@") check bobj('f -> ".", 'g -> 1, 'h -> ".", 'p -> bobj('pf -> "" , 'pg -> 2))
    in.translate(_.ifStringRecursively).usingLenient("" -> ".", "@@@" -> "@") check bobj('f -> ".", 'g -> 1, 'h -> ".", 'p -> bobj('pf -> ".", 'pg -> 2))

    in.translate(_.soleKey).usingLenient("" -> 0) metaError MoreThanOneKey
    //bobj('f -> "", 'g -> 1, 'h -> "", 'p -> bobj('pf -> "", 'pg -> 2))
    in.translate(_.ifString           ).usingLenient("" -> 0) metaError ("MustBeSameType", "-")
    in.translate(_.ifString           ).usingStrict ("" -> 0) check bobj('f -> 0, 'g -> 1, 'h -> 0, 'p -> bobj('pf -> "", 'pg -> 2))
    in.translate(_.ifStringRecursively).usingStrict ("" -> 0) check bobj('f -> 0, 'g -> 1, 'h -> 0, 'p -> bobj('pf ->  0, 'pg -> 2))
    //in.applyRecursivelyIfValue[String](_.startsWith("f")).using(_.toUpperCase)
  }

  // ---------------------------------------------------------------------------
  private def testTranslate3() {
    Default10.translate(_.soleKey).usingLenient("foo" -> "oof") metaError MoreThanOneKey
    Default11.translate(_.soleKey).usingLenient("foo" -> "oof") check bobj('f -> "oof")
    Default11.translate(_.soleKey).usingStrict ("foo" -> 3    ) check bobj('f -> 3)

    Default06.translate(_.indices(0,  1)).using("foo" -> "oof") check bobj('f1 -> "oof", 'f2 -> "oof", 'g -> 1)
    Default06.translate(_.indices(0, -2)).using("foo" -> "oof") check bobj('f1 -> "oof", 'f2 -> "oof", 'g -> 1)
    Default06.translate(_.indices(0,  5)).using("foo" -> "oof") metaError OutOfBoundKey
    Default06.translate(_.indices(0, -5)).using("foo" -> "oof") metaError OutOfBoundKey
//Default06.translate(_.indices(0,  5)).using("foo" -> "oof") fail OutOfBound
//Default06.translate(_.indices(0, -5)).using("foo" -> "oof") fail OutOfBound

    Default03.translate('p |> 'f).using("foo" -> "oof") check bobj('p ->     Default01c,              'z -> true)
    Default04.translate('p |> 'f).using("foo" -> "oof") check bobj('p -> Seq(Default01c, Default01b), 'z -> true)

    Default03.translate('p |> 'f ~> 'F).using("foo" -> "oof") check bobj('p -> bobj('F -> "oof", 'g -> 1), 'z -> true)
  }

  // ===========================================================================
  private def testSwap() {
    Default01                             .swapEntries          ('f, "g" ) check            bobj('g -> "foo", 'f -> 1)
    Default03.transform(_.obj('p)).using(_.swapEntries          ('f, "g")) check bobj('p -> bobj('g -> "foo", 'f -> 1), 'z -> true)
    Default03                             .swapEntries('p ~> 'P)('f, "g" ) check bobj('P -> bobj('g -> "foo", 'f -> 1), 'z -> true)

    // ---------------------------------------------------------------------------
    bobj('f1 -> "foo1", 'f2 -> "foo2", 'g1 -> 1, 'g2 -> 2)
      .swapEntries(
          'f1  -> 'g1 ,
          "f2" -> 'g2)
        .check(
            bobj('g1 -> "foo1", 'g2 -> "foo2", 'f1 -> 1, 'f2 -> 2))
  }

  // ===========================================================================
  private def testCopy() {
    Default01.copyEntry('f      ).as('f2     ) check bobj('f -> "foo", 'g -> 1, 'f2 -> "foo")
    Default01.copyEntry('f      ).as('f2, 'f3) check bobj('f -> "foo", 'g -> 1, 'f2 -> "foo", 'f3 -> "foo")
    Default01.copyEntry('f ~> 'F).as('f2, 'f3) check bobj('F -> "foo", 'g -> 1, 'f2 -> "foo", 'f3 -> "foo")

    // ---------------------------------------------------------------------------
    Default03.copyEntry('p |> 'f ~> 'F).as('f2, 'f3) check bobj('p -> bobj('F -> "foo", 'g -> 1, 'f2 -> "foo", 'f3 -> "foo"), 'z -> true)
  }

  // ===========================================================================
  private def testMiscStringOps(in: BObj) {
    in.transform(_.string('f)).using(_.toUpperCase)      check bobj('f -> "FOO", 'g -> 1)
    in                                .toUpperCase  ('f) check bobj('f -> "FOO", 'g -> 1)
    in                                .reverseString('f) check bobj('f -> "oof", 'g -> 1)

    TestDataO.Default15m.noop(_.toUpperCase('f))
  }

}

// ===========================================================================
