package galliatest.suites.single

import aptus.String_ // for .date
import gallia._

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

    val XXp = aobj(cls('f.string, 'g.int_))(obj('f -> "foo", 'g ->  1 ))
    val XXm = aobj(cls('f.string, 'g.int_))(obj('f -> "foo"           ))

    Default01.convert('g).toNonRequired                  .check(XXp)
    Default01.convert('g).toNonRequired(strict = true)   .check(XXp)
    Default01.customField('g).using(_.toNonRequired)     .check(XXp)
    Default01.customU2U(_.toNonRequired('g), identity)   .check(XXp)

    XXp.convert('g).toRequired               .check(Default01)
    XXp.convert('g).toRequired(strict = true).check(Default01)
    XXm.convert('g).toRequired               .dataError("201016153348" -> "")
    XXm.convert('g).toRequired(strict = true).dataError("201016153348" -> "")

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
    val xxx = bobj('p -> Seq(
        bobj('f -> "a", 'g -> "1"),
        bobj('f -> "b", 'g -> "2"),
        bobj('f -> "c", 'g -> "3")))

    xxx.transformObjects('p).using(_.convert('f)    .toRequired).check(xxx)
    xxx.transformObjects('p).using(_.convert('f, 'g).toRequired).check(xxx)

    if (false) { // getting error 210106171801 --> FIXME: t210110203020 - needs to be able to deal with multiplicity in nesting(s)
      xxx.convert('p |> 'f)          .toRequired
      xxx.convert('p |> 'f, 'p |> 'g).toRequired }

    bobj('p -> bobj('f -> "a", 'g -> "1")).noop(_.convert('p |> 'f)          .toRequired)
    bobj('p -> bobj('f -> "a", 'g -> "1")).noop(_.convert('p |> 'f, 'p |> 'g).toRequired)


    XXm.convert('g).toRequired.dataError("NotDefined" -> "")

    // ---------------------------------------------------------------------------
    // FIXME - t210301103745: Default01.remove.removeIfValueFor('g).is(1).convert('g).toStr
  }

}

// ===========================================================================