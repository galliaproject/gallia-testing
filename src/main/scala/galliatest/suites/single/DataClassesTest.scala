package galliatest.suites.single

import aptus._ // for .as.noneIf
import gallia._

// ===========================================================================
object DataClassesTest extends gallia.testing.Suite { // formerly CcMappingTest
	import TestDataO._
	import TestMeta._

	// ---------------------------------------------------------------------------
  override def test(): Unit = {
    Default03 .transformDataClass[           _Default01 ] ("p").using(            _.op)                     .check(bobj("p" ->  4.4, "z" -> true))
    Default03p.transformDataClass[Option[    _Default01 ]]("p").using(      _.map(_.op)     .getOrElse(1.1)).check(bobj("p" ->  4.4, "z" -> true))
    Default03m.transformDataClass[Option[    _Default01 ]]("p").using(      _.map(_.op)     .getOrElse(1.1)).check(bobj("p" ->  1.1, "z" -> true))
    Default04 .transformDataClass[       Seq[_Default01 ]]("p").using(      _.map(_.op).sum)                .check(bobj("p" -> 11.0, "z" -> true)) // 4.4+6.6=11.0
    Default04p.transformDataClass[Option[Seq[_Default01]]]("p").using(_.map(_.map(_.op).sum).getOrElse(1.1)).check(bobj("p" -> 11.0, "z" -> true)) // 4.4+6.6=11.0
    Default04m.transformDataClass[Option[Seq[_Default01]]]("p").using(_.map(_.map(_.op).sum).getOrElse(1.1)).check(bobj("p" ->  1.1, "z" -> true))

    // ---------------------------------------------------------------------------
    Default03.transformDataClass[_Default01]("p").using(x => f_String(x.f)).check(bobj("p" -> bobj("f" -> "foo"), "z" -> true))

    // ---------------------------------------------------------------------------
    { //_.transformDataClass[Seq[AlleleData]](alleles).using {
      val expected = bobj("p" -> bobj(/*"g" -> 1, */"f" -> 3, "h" -> false), "z" -> true)

      // ---------------------------------------------------------------------------
      Default04.transformDataClass[Seq[_Default01]]("p")
            .using { dc =>
              f_Int$h_Boolean(dc.head.f.size, (dc.head.f.size % 2 ) == 0) }
          .check(expected)

        // ---------------------------------------------------------------------------
        // convoluted way
        Default04.cotransformViaDataClass[__Default01]
            .using { _.p.head.pipe { head => $f_Int$h_Boolean(f_Int$h_Boolean(head.f.size, (head.f.size % 2 ) == 0)) } }
          .check(expected.reverseKeyOrder.forceAObj)
    }
	}

  // ===========================================================================
  Default01 .cotransformViaDataClass[f_String]  .using { dc => f2_String  (dc.f      .toUpperCase)  }.check(bobj('f ->     "foo" ,          'g -> 1, 'f2 ->     "FOO") )
  Default02 .cotransformViaDataClass[f_Strings] .using { dc => f2_Strings (dc.f.map(_.toUpperCase)) }.check(bobj('f -> Seq("foo1", "foo2"), 'g -> 1, 'f2 -> Seq("FOO1", "FOO2")) )

  Default13p.cotransformViaDataClass[f_String_] .using { dc => f2_String_ (dc.f.map(_.toUpperCase)) }.check(aobj(cls('f.string_, 'g.int, 'f2.string_))(obj('f -> "foo", 'g -> 1, 'f2 -> "FOO")))
  Default13m.cotransformViaDataClass[f_String_] .using { dc => f2_String_ (dc.f.map(_.toUpperCase)) }.check(aobj(cls('f.string_, 'g.int, 'f2.string_))(obj(             'g -> 1)))

  Default14p.cotransformViaDataClass[f_Strings_].using { dc => f2_Strings_(dc.f.map(_.map(_.toUpperCase))) }.check(aobj(cls('f.strings_, 'g.int, 'f2.strings_))(obj('f -> Seq("foo1", "foo2"), 'g -> 1, 'f2 -> Seq("FOO1", "FOO2"))))
  Default14m.cotransformViaDataClass[f_Strings_].using { dc => f2_Strings_(dc.f.map(_.map(_.toUpperCase))) }.check(aobj(cls('f.strings_, 'g.int, 'f2.strings_))(obj(                           'g -> 1)))

  // ===========================================================================
  Default01 .cotransformViaDataClass[f_String]  .as("f2").using(_.f.size).check(bobj('f -> "foo" , 'g -> 1, 'f2 -> 3) )

  Default02 .cotransformViaDataClass[f_Strings] .as("f2").using(_.f.size).check(bobj('f -> Seq("foo1", "foo2"), 'g -> 1, 'f2 -> 2) )

  Default13p.cotransformViaDataClass[f_String_] .as("f2").using(_.f.map(_.size).getOrElse(0)).check(aobj(cls('f.string_, 'g.int, 'f2.int ))(obj('f -> "foo", 'g -> 1, 'f2 -> 3)))
  Default13p.cotransformViaDataClass[f_String_] .as("f2").using(_.f.map(_.size)             ).check(aobj(cls('f.string_, 'g.int, 'f2.int_))(obj('f -> "foo", 'g -> 1, 'f2 -> 3)))
  Default13m.cotransformViaDataClass[f_String_] .as("f2").using(_.f.map(_.size).getOrElse(0)).check(aobj(cls('f.string_, 'g.int, 'f2.int ))(obj(             'g -> 1, 'f2 -> 0)))
  Default13m.cotransformViaDataClass[f_String_] .as("f2").using(_.f.map(_.size)             ).check(aobj(cls('f.string_, 'g.int, 'f2.int_))(obj(             'g -> 1)))

  Default14p.cotransformViaDataClass[f_Strings_].as("f2").using(_.f.map(_.size).getOrElse(0)).check(aobj(cls('f.strings_, 'g.int, 'f2.int ))(obj('f -> Seq("foo1", "foo2"), 'g -> 1, 'f2 -> 2)))
  Default14p.cotransformViaDataClass[f_Strings_].as("f2").using(_.f.map(_.size)             ).check(aobj(cls('f.strings_, 'g.int, 'f2.int_))(obj('f -> Seq("foo1", "foo2"), 'g -> 1, 'f2 -> 2)))
  Default14m.cotransformViaDataClass[f_Strings_].as("f2").using(_.f.map(_.size).getOrElse(0)).check(aobj(cls('f.strings_, 'g.int, 'f2.int ))(obj(                           'g -> 1, 'f2 -> 0)))
  Default14m.cotransformViaDataClass[f_Strings_].as("f2").using(_.f.map(_.size)             ).check(aobj(cls('f.strings_, 'g.int, 'f2.int_))(obj(                           'g -> 1)))

  // ---------------------------------------------------------------------------
  // with removal
  Default01.cotransformViaDataClass[f_String] .as("f2").usingWithErasing(_.f.size).check(bobj(              'g -> 1, 'f2 -> 3) )

  // ===========================================================================
  Default01
    .cotransformViaDataClass[f_String]
      .usingWithErasing { dc => f2_Int$h_Boolean(dc.f.size, (dc.f.size % 2 ) == 0) }
        .check(bobj("g" -> 1, "f2" -> 3, "h" -> false) )

if (false)
    TestDataO.Default03 //val Default03  = bobj('p -> Default01 , 'z -> true)
      .transformDataClass[f_String]("p")
        .using { dc => Quux2c(dc.f.size, (dc.f.size % 2 ) == 0) }
          .check(bobj("p" -> bobj(/*"g" -> 1, */"f" -> 3, "h" -> false), "z" -> true) ) // --> differs from v1... (g field)

  // ===========================================================================
  { // deprecated way now, c220914145147 or t220914144458 instead

    // manually would be: nest 'f under 'g, rename 'g |> 'f as 'a, then generate 'g |> 'A from 'a
    Default01   .transform(_.string ('f)).using { s =>     Foo(s, s.toUpperCase)                               }.check(bobj('f ->     bobj('a -> "foo", 'A -> "FOO")                                                   , 'g -> 1))
    Default01   .transform(_.stringx('f)).using { s =>     Foo(s, s.toUpperCase)                               }.check(bobj('f ->     bobj('a -> "foo", 'A -> "FOO")                                                   , 'g -> 1))
    Default01   .transform(_.string ('f)).using { s => Seq(Bar(s, s.toUpperCase, 1), Bar(s, s.toUpperCase, 2)) }.check(bobj('f -> Seq(bobj('a -> "foo", 'A -> "FOO", 'i -> 1), /*a*/bobj('a -> "foo", 'A -> "FOO", 'i -> 2)), 'g -> 1))

    Default01   .transform(_.string('f)).using { s => s.in.noneIf(_.startsWith("z")).map(s2 => Foo(s2, s2.toUpperCase)) }.check(
      aobj(
        cls('f  .cls_('a.string, 'A.string), 'g.int))(
        obj('f -> obj('a -> "foo", 'A -> "FOO"), 'g -> 1)))

    // will remove it
    Default01   .transform(_.string('f)).using { s => s.in.noneIf(_.startsWith("f")).map(s2 => Foo(s2, s2.toUpperCase)) }.check(
      aobj(
        cls('f  .cls_('a.string, 'A.string), 'g.int))(
        obj(                                 'g -> 1)) )
  }

}

// ===========================================================================
