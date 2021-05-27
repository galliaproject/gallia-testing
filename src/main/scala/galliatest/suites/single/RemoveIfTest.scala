package galliatest.suites.single

import gallia._

// ===========================================================================
object RemoveIfTest extends gallia.testing.Suite {
  import vldt._Error.TypeMismatch

private val tmp201224  = aobj(cls('f.string_, 'g.int))(obj(             'g -> 1))
private val tmp201224b = aobj(cls('f.string_, 'g.int))(obj('f -> "foo", 'g -> 1))
import TestDataO._    
  
  // ===========================================================================
  override def test() { //import TestDataO.{Default01, Default01c, Default03, Default04, Default06, Default10, Default11}
    if (false) { // compiles
      Default01.removeIfValueFor('p1 |> 'f, 'p2 |> 'g ~> 'G)
      Default01.forEachPath(_.explicit('p1 |> 'f, 'p2 |> 'g/* ~> 'G*/)).zen(_.removeIfValueFor(_).is("foo"))
    }

    Default01.removeIfValueFor(_.firstKey          ).is     ("foo")            .check(tmp201224) // whatever
    Default01.removeIfValueFor(_.string('f)        ).matches(_.startsWith("f")).check(tmp201224)
    Default01.removeIfValueFor(_.string('f)        ).matches(_.startsWith("f")).check(tmp201224)
    Default01.removeIfValueFor(_.string(_.firstKey)).matches(_.startsWith("f")).check(tmp201224)

    Default10.removeIfValueFor(_.ifString).is("").check(
      aobj(
        cls('f.string_, 'g.int , 'h.string_, 'p.cls   ('pf.string, 'pg.int )))(
        obj(            'g -> 1,             'p -> obj('pf -> "" , 'pg -> 2)) ) )

    Default10.removeIfValueFor(_.ifStringRecursively).is("").check(
      aobj(
        cls('f.string_, 'g.int , 'h.string_, 'p.cls   ('pf.string_, 'pg.int )))(
        obj(            'g -> 1,             'p -> obj(             'pg -> 2)) ) )

    //Default10.removeIfValueFor(_.allStringPaths).is("").check()
    //Default10.removeIfValueFor(_.allStringPaths).isEmptyString.check()
    if (false) Default10.removeIfValueFor(_.allKeys).is("") // uses Whatever so doesn't error out on type - FIXME: bad...
      // same for this? Default10.removeIfValueFor(_.allKeysRecursively)//.isEmptyString.check()

    Default01                         .removeIfValueFor(_.string(_.allKeys)).is("foo" ).metaError[TypeMismatch]
    bobj('f1 -> "foo1", 'f2 -> "foo2").removeIfValueFor(_.string(_.allKeys)).is("foo2")
      .check(
        aobj(
          cls('f1.string_  , 'f2.string_))(
          obj('f1 -> "foo1") ))

    val ttt1 =
      aobj(
          cls('f1.string_ , 'f2.string, 'g.int))(
          obj('f2 -> "foo", 'g -> 1) )

    val ttt2 =
      aobj(
          cls('f1.string_ , 'f2.string_, 'g.int))(
          obj(                           'g -> 1) )

    Default06.removeIfValueFor(_.string(_.firstKey                     )).matches(_.startsWith("f")).check(ttt1)
    Default06.removeIfValueFor(_.string(_.filterKeys(_.startsWith("f")))).matches(_.startsWith("f")).check(ttt2)

    Default06.removeIfValueFor { _.string(_.allKeys) }.matches(_.startsWith("f")).metaError[TypeMismatch]

    Default01        .removeIfValueFor(_.string('f)).is("foo").check(tmp201224)
  //Default01        .removeIfValueFor(_.$     ('f)).is("foo").check(tmp201224)
    Default01        .removeIfValueFor(         'f ).is("foo").check(tmp201224)

    Default01        .removeIfValueFor('f).is("foo").check(tmp201224)
    Default01        .removeIfValueFor('f).is("FOO").check(tmp201224b)

    Default02        .removeIfValueFor('f).is(Seq("foo1", "foo2")).check(aobj(cls('f.strings_, 'g.int))(obj(                           'g -> 1)))
    Default02        .removeIfValueFor('f).is(Seq("foo1", "FOO2")).check(aobj(cls('f.strings_, 'g.int))(obj('f -> Seq("foo1", "foo2"), 'g -> 1)))

    Default01        .removeIfValueFor(_.string('f)).matches(_.startsWith("f")).check(tmp201224)
    Default01        .removeIfValueFor(_.string('f)).matches(_.startsWith("F")).check(tmp201224b)

  //Default01              .removeIfValueFor('f).isEmptyString.check(tmp201224b)
  //Default01d             .removeIfValueFor('f).isEmptyString.check(tmp201224)
    Default01              .removeIfValueFor('f).is("").check(tmp201224b)
    bobj('f -> "", 'g -> 1).removeIfValueFor('f).is("").check(tmp201224)

    Default01              .removeIfValueFor('f).is("").check(tmp201224b)
    bobj('f -> "", 'g -> 1).removeIfValueFor('f).is("").check(tmp201224)

    Default06.removeIfValueFor(_.string('f1, 'f2)).is("foo").check(aobj(cls('f1.string_, 'f2.string_, 'g.int))(obj('g -> 1)))

    Default01                         .removeIfValueFor(_.string('f , 'g )).is("foo").metaError[TypeMismatch]
    bobj('f1 -> "foo" , 'f2 -> "foo" ).removeIfValueFor(_.string('f1, 'f2)).is("foo").dataError[vldt._Error.ObjCantBeEmpty.type]

    Default15p         .removeIfValueFor('f).is("foo").check(aobj(cls('f.string_, 'g.int, 'h.boolean))(obj('g -> 1, 'h -> true)))
    Default15m.noop { _.removeIfValueFor('f).is("foo") }
    Default15m.noop { _.removeIfValueFor('f).is("FOO") }
    Default15p.noop { _.removeIfValueFor('f).is("FOO") }
    Default15m.noop { _.removeIfValueFor('f).is(None) } // bad, TODO: t210111134021 - forbid

    Default16p         .removeIfValueFor('f).is(Seq("foo1", "foo2")).check(aobj(cls('f.strings_, 'g.int, 'h.boolean))(obj('g -> 1, 'h -> true)))
    Default16m.noop { _.removeIfValueFor('f).is(Seq("foo1", "foo2")) }
    Default16m.noop { _.removeIfValueFor('f).is(Seq("FOO1", "FOO2")) }
    Default16p.noop { _.removeIfValueFor('f).is(Seq("FOO1", "FOO2")) }
    Default16m.noop { _.removeIfValueFor('f).is(None) } // bad, TODO: t210111134021 - forbid

    // ---------------------------------------------------------------------------

    //disallowed
  //Default01.removeIfValueFor(_.$       ('f))            .is("foo")                .check(PPPP1)
  //Default01.removeIfValueFor(_.explicit('f))            .is("foo")                .check(PPPP1)

    Default01.removeIfValueFor('f)                        .is("foo")                 .check(Default13m)
    Default01.removeIfValueFor(_.firstKey)                .is("foo")                 .check(Default13m)
    Default01.removeIfValueFor(_.index(0))                .is("foo")                 .check(Default13m)
    Default01.removeIfValueFor(_.allButLast  )            .is("foo")                 .check(Default13m)
    Default01.removeIfValueFor(_.allKeys)                 .is("foo")                 .check(aobj(cls('f.string_, 'g.int_  ))(obj('g -> 1)))
    Default01.removeIfValueFor(_.string(_.ifType[String])).matches(_.startsWith("f")).check(Default13m)
    Default01.removeIfValueFor(_.string(_.ifString      )).matches(_.startsWith("f")).check(Default13m)
    Default01.removeIfValueFor(_.allStringKeys )          .matches(_.startsWith("f")).check(Default13m)
    Default01.removeIfValueFor(_.allStringPaths)          .matches(_.startsWith("f")).check(Default13m)
    Default03.removeIfValueFor(_.allStringPaths)          .matches(_.startsWith("f")).check(AObj(cls('p.cls(Default13m.c), 'z.boolean), obj('p -> Default13m.u, 'z -> true)))

//    bobj('f -> "", 'g -> 1).removeIfValueFor(_.string('f)).isEmptyString.check(PPPP1)
//    bobj('f -> "", 'g -> 1).removeIfValueFor(         'f ).isEmptyString.check(PPPP1)
//
//    bobj('f -> "", 'g -> 0).removeIfValueFor(_.int('g)).isZero.check(PPPP1)
//    bobj('f -> "", 'g -> 0).removeIfValueFor(      'g ).isZero.check(PPPP1)


    Default13p.removeIfValueFor(_.string('f))                       .matches(_.startsWith("f")).check(Default13m)

//    PPPP1.removeIfValueFor(_.allOptionalStringKeys)            .matches(_.exists(_.startsWith("f"))).check(PPPP1) // already missing
//    PPPP3.removeIfValueFor(_.allOptionalStringKeys)            .matches(_.exists(_.startsWith("f"))).check(PPPP1)
//    PPPP3.removeIfValueFor(_.string_(_.ifType[Option[String]])).matches(_.exists(_.startsWith("f"))).check(PPPP1)
    Default13m.removeIfValueFor(_.allStringKeys)           .matches(_.startsWith("f")).check(Default13m) // already missing
    if (false) { // FIXME: probably fails to consider Option[String] as string
      Default13p.removeIfValueFor(_.allStringKeys)           .matches(_.startsWith("f")).check(Default13m)
      Default13p.removeIfValueFor(_.string(_.ifType[String])).matches(_.startsWith("f")).check(Default13m)
    }

    Default13p.removeIfValueFor('f).is("foo").check(Default13m)
    Default13m.removeIfValueFor('f).is("foo").check(Default13m)

    Default02.removeIfValueFor(_.strings('f)).matches(_.exists(_.endsWith("1"))).check(Default14m)
    Default02.removeIfValueFor(_.strings('f)).matches(_.exists(_.endsWith("3"))).check(Default14p)

    Default14p.removeIfValueFor(_.strings('f)).matches(_.exists(_.endsWith("1"))).check(Default14m)
    Default14p.removeIfValueFor(_.strings('f)).matches(_.exists(_.endsWith("3"))).check(Default14p) // no-op
    Default14m.removeIfValueFor(_.strings('f)).matches(_.exists(_.endsWith("1"))).check(Default14m) // no-op

if (false) { // FIXME: 210108095411
    Default14p.removeIfValueFor(_.strings('f)).matches(_.exists(_.endsWith("3"))).metaError[TypeMismatch]
}
  // ---------------------------------------------------------------------------
  Default01.removeIfValueFor(_.int     ('g)).matches(_ => true).check(aobj(cls('f.string, 'g.int_))(obj('f -> "foo"))) // via tautology
//Default01.removeIfValueFor(_.$       ('g)).matches(_ => true).check(aobj(cls('f.string, 'g.int_))(obj('f -> "foo")))
//Default01.removeIfValueFor(_.explicit('g)).matches(_ => true).check(aobj(cls('f.string, 'g.int_))(obj('f -> "foo")))
//Default01.removeIfValueFor(_.any     ('g)).matches(_ => true).check(aobj(cls('f.string, 'g.int_))(obj('f -> "foo")))
//Default01.removeIfValueFor(_.typed[Any]('g)).matches(_ => true).check(aobj(cls('f.string, 'g.int_))(obj('f -> "foo"))) //Exception in thread "main" java.lang.IllegalArgumentException: ([Err( (200701145247,UnsupportedTlSubtype) - <root> - scala.Any)],digraph default {

    // ---------------------------------------------------------------------------
    Default01 .removeIfValueFor('f).is("foo").check(Default13m)
    Default13m.setDefaultFor   ('f). asValue("foo").check(Default01)

    Default02 .removeIfValueFor('f).is     (Seq("foo1", "foo2")).check(Default14m)
    Default14m.setDefaultFor   ('f).asValue(Seq("foo1", "foo2")).check(Default02)

    Default13p.removeIfValueFor('f).is("foo").check(Default13m)
    Default13m.removeIfValueFor('f).is("foo").check(Default13m)
    Default13p.removeIfValueFor('f).is("FOO").check(Default13p)

    Default14p.removeIfValueFor('f).is(Seq("foo1", "foo2")).check(Default14m)
    Default14m.removeIfValueFor('f).is(Seq("foo1", "foo2")).check(Default14m)
    Default14p.removeIfValueFor('f).is(Seq("FOO1", "foo2")).check(Default14p)

    Default01.removeIfValueFor(_.int('g)).matches(_ => true).check(aobj(cls('f.string, 'g.int_))(obj('f -> "foo"))) // via tautology
  }

}

// ===========================================================================
