package galliatest.suites.single

import aptus.String_ // for .date
import gallia._
import gallia.vldt._Error.Runtime

// ===========================================================================
object ConvertTest extends gallia.testing.Suite {
	import TestDataO._

	// ---------------------------------------------------------------------------
  override def test() {

    Default01.convert('g).toStr.check(bobj('f -> "foo", 'g -> "1"))

    bobj('f -> -1    , 'g -> 1).convert('f).toBoolean(-1 ,  1 ).check(bobj('f -> true, 'g -> 1))
    bobj('f -> "T"   , 'g -> 1).convert('f).toBoolean("T", "F").check(bobj('f -> true, 'g -> 1))
    bobj('f -> "true", 'g -> 1).convert('f).toBoolean          .check(bobj('f -> true, 'g -> 1))

    Default03.convert('p |> 'g).toDouble.check(bobj('p ->     bobj('f -> "foo", 'g -> 1.0)                                , 'z -> true))
    Default04.convert('p |> 'g).toDouble.check(bobj('p -> Seq(bobj('f -> "foo", 'g -> 1.0), bobj('f -> "foo2", 'g -> 2.0)), 'z -> true))

    val outputPresent = aobj(cls('f.string, 'g.int_))(obj('f -> "foo", 'g ->  1 ))
    val outputMissing = aobj(cls('f.string, 'g.int_))(obj('f -> "foo"           ))

    Default01.convert('g).toNonRequired                  .check(outputPresent)
    Default01.convert('g).toNonRequired(strict = true)   .check(outputPresent)
    Default01.customField('g).using(_.toNonRequired)     .check(outputPresent)
    Default01.customU2U(_.toNonRequired('g), identity)   .check(outputPresent)

    outputPresent.convert('g).toRequired               .check(Default01)
    outputPresent.convert('g).toRequired(strict = true).check(Default01)
    outputMissing.convert('g).toRequired               .dataError[Runtime.NotDefined]
    outputMissing.convert('g).toRequired(strict = true).dataError[Runtime.NotDefined]

    Default01.convert('g).toMultiple               .check(bobj('f -> "foo", 'g -> Seq(1)))
    Default01.convert('g).toMultiple(strict = true).check(bobj('f -> "foo", 'g -> Seq(1)))

    bobj('f -> "foo", 'g -> Seq(1)).convert('g).toNonMultiple               .check(Default01)
    bobj('f -> "foo", 'g -> Seq(1)).convert('g).toNonMultiple(strict = true).check(Default01)


    bobj('f -> "foo", 'g -> "1.00").convert('g).toInt.check(Default01)
    bobj('f -> "foo", 'g -> "1"   ).convert('g).toInt.check(Default01)
    Default01                      .convert('g).toStr   .check(bobj('f -> "foo", 'g -> "1"))
    Default01                      .convert('g).toDouble.check(bobj('f -> "foo", 'g -> 1.0))
    bobj('f -> "foo", 'g -> "true").convert('g).toBoolean                  .check(bobj('f -> "foo", 'g -> true))
    Default01                      .convert('g).toBoolean            (1, 0).check(bobj('f -> "foo", 'g -> true))
    Default01                      .convert('g).toOptionalBoolean(-1)(1, 0).check(aobj(
        cls('f.string  , 'g.boolean_))(
        obj('f -> "foo", 'g -> true )))

    Default01                      .convert('g).toOptionalBoolean( 1)(-1, 0).check(aobj(
        cls('f.string  , 'g.boolean_))(
        obj('f -> "foo")))

    Default01                      .convert('g).toFlag(1).check(aobj(
        cls('f.string  , 'g.boolean_))(
        obj('f -> "foo", 'g -> true )))

    // ---------------------------------------------------------------------------
    // flag

    Default01 .convert('f).toFlag("foo")              .check(aobj(cls('f.boolean_, 'g.int))(obj('f -> true, 'g -> 1)))
    Default02 .convert('f).toFlag(Seq("foo1", "foo2")).check(aobj(cls('f.boolean_, 'g.int))(obj('f -> true, 'g -> 1)))
    Default13p.convert('f).toFlag("foo")              .check(aobj(cls('f.boolean_, 'g.int))(obj('f -> true, 'g -> 1)))
    Default14p.convert('f).toFlag(Seq("foo1", "foo2")).check(aobj(cls('f.boolean_, 'g.int))(obj('f -> true, 'g -> 1)))

    Default01 .convert('f).toFlag("FOO")              .check(aobj(cls('f.boolean_, 'g.int))(obj(            'g -> 1)))
    Default02 .convert('f).toFlag(Seq("FOO1", "foo2")).check(aobj(cls('f.boolean_, 'g.int))(obj(            'g -> 1)))
    Default13p.convert('f).toFlag("FOO")              .check(aobj(cls('f.boolean_, 'g.int))(obj(            'g -> 1)))
    Default14p.convert('f).toFlag(Seq("FOO1", "foo2")).check(aobj(cls('f.boolean_, 'g.int))(obj(            'g -> 1)))
    Default13m.convert('f).toFlag("foo")              .check(aobj(cls('f.boolean_, 'g.int))(obj(            'g -> 1)))
    Default14m.convert('f).toFlag(Seq("foo1", "foo2")).check(aobj(cls('f.boolean_, 'g.int))(obj(            'g -> 1)))

    bobj('f ->  3 , 'g -> 1).convert('f).toFlag(3).check(aobj(cls('f.boolean_, 'g.int))(obj('f -> true, 'g -> 1)))
    bobj('f -> "3", 'g -> 1).convert('f).toInt    .check(bobj('f -> 3, 'g -> 1))

    Default01 .transform(_.stringx('f)).using(_.size.toString).convert('f).toInt.check(aobj(cls('f.int  , 'g.int))(obj('f -> 3        , 'g -> 1)))
    Default02 .transform(_.stringx('f)).using(_.size.toString).convert('f).toInt.check(aobj(cls('f.ints , 'g.int))(obj('f -> Seq(4, 4), 'g -> 1)))
    Default13p.transform(_.stringx('f)).using(_.size.toString).convert('f).toInt.check(aobj(cls('f.int_ , 'g.int))(obj('f -> 3        , 'g -> 1)))
    Default13m.transform(_.stringx('f)).using(_.size.toString).convert('f).toInt.check(aobj(cls('f.int_ , 'g.int))(obj(                 'g -> 1)))
    Default14p.transform(_.stringx('f)).using(_.size.toString).convert('f).toInt.check(aobj(cls('f.ints_, 'g.int))(obj('f -> Seq(4, 4), 'g -> 1)))
    Default14m.transform(_.stringx('f)).using(_.size.toString).convert('f).toInt.check(aobj(cls('f.ints_, 'g.int))(obj(                 'g -> 1)))

    // ---------------------------------------------------------------------------
    bobj('value -> new java.lang.Long(3)).convert('value).toInt.check(bobj('value -> 3))

    bobj('value -> BigInt    (3)).convert('value).toInt.check(bobj('value -> 3))
    bobj('value -> BigDecimal(3)).convert('value).toInt.check(bobj('value -> 3))
    bobj('value -> scala.BigDecimal(3)).convert('value).toInt.check(bobj('value -> 3))
    bobj('value -> "2021-01-08".date).transform(_.date('value)).using(_.getYear.toString).convert('value).toInt.check(bobj('value -> 2021))

    // ---------------------------------------------------------------------------
    val output3 = bobj('p -> Seq(
        bobj('f -> "a", 'g -> "1"),
        bobj('f -> "b", 'g -> "2"),
        bobj('f -> "c", 'g -> "3")))

    val output4p = aobj('p.clss('f.string_, 'g.string))(obj('p -> Seq(
        obj('f -> "a", 'g -> "1"),
        obj('f -> "b", 'g -> "2"),
        obj('f -> "c", 'g -> "3"))))

    val output4m = aobj('p.clss('f.string_, 'g.string))(obj('p -> Seq(
        obj('f -> "a", 'g -> "1"),
        obj(           'g -> "2"),
        obj('f -> "c", 'g -> "3"))))

    output3.transformObjects('p).using(_.convert('f)    .toRequired).check(output3)
    output3.transformObjects('p).using(_.convert('f, 'g).toRequired).check(output3)
    
    output3.noop(_.convert('p |> 'f)          .toRequired)
    output3.noop(_.convert('p |> 'f, 'p |> 'g).toRequired)

    output4p.convert('p |> 'f).toRequired.check(output3)
    output4m.convert('p |> 'f).toRequired.dataError[Runtime.NotDefined]

    bobj('p -> bobj('f -> "a", 'g -> "1")).noop(_.convert('p |> 'f)          .toRequired)
    bobj('p -> bobj('f -> "a", 'g -> "1")).noop(_.convert('p |> 'f, 'p |> 'g).toRequired)

    outputMissing.convert('g).toRequired.dataError("NotDefined" -> "")

    // ---------------------------------------------------------------------------
    Default01.removeIfValueFor('g).is(1)                  .check(aobj(
        cls('f.string  , 'g.int_))(
        obj('f -> "foo")))
    Default01.removeIfValueFor('g).is(1).convert('g).toStr.check(aobj(
        cls('f.string  , 'g.string_))(
        obj('f -> "foo")))

    Default02 .noop(_.convert('f).toStr)    
    Default13p.noop(_.convert('f).toStr)
    Default13m.noop(_.convert('f).toStr)
    Default14p.noop(_.convert('f).toStr)
    Default14m.noop(_.convert('f).toStr)
    
    Default13p.transformString('f).using(_.size).convert('f).toStr.check(aobj(cls('f.string_ , 'g.int))(obj('f ->     "3"      , 'g -> 1)))
    Default13m.transformString('f).using(_.size).convert('f).toStr.check(aobj(cls('f.string_ , 'g.int))(obj(                     'g -> 1)))
    Default14p.transformString('f).using(_.size).convert('f).toStr.check(aobj(cls('f.strings_, 'g.int))(obj('f -> Seq("4", "4"), 'g -> 1)))
    Default14m.transformString('f).using(_.size).convert('f).toStr.check(aobj(cls('f.strings_, 'g.int))(obj(                     'g -> 1)))
  }

}

// ===========================================================================
