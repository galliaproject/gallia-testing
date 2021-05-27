package galliatest.suites.single

import aptus._
import gallia._

// ===========================================================================
object FuseFissionTest extends gallia.testing.Suite {

  // ===========================================================================
  override def test() {
import TestDataO._//{Default01, Default01c, Default03, Default04, Default06, Default10, Default11}  
import galliatest.TestMeta._
//import galliatest.TestData.{Baz1, Baz2, Baz3, Baz4}
    Default01
      .fission(
          _.string('f))
        .as('f1, 'f2)
          .using { s => (
              s.head.str,
              s.tail) }
        .check(bobj('g -> 1, 'f1 -> "f", 'f2 -> "oo"))

    // ===========================================================================
    def tmp9(x: AObj): HeadU =
      x
        .fuse(
            _.string_('f),
            _.int    ('g))
          .as('fg)
            .using { (f, g) =>
              f.map(_.underscore(g.str)) }

      tmp9(Default15m)
        .check(aobj(
            cls('h.boolean, 'fg.string_))(
            obj('h -> true) ))

      tmp9(Default15p)
        .check(aobj(
            cls('h.boolean, 'fg.string_))(
            obj('h -> true, 'fg -> "foo_1") ))

      tmp9(aobj(
          cls('f.string_, 'g.int ))(
          obj(            'g -> 1)))
        .dataError[vldt._Error.ObjCantBeEmpty.type]

    // ===========================================================================
    def tmp8(x: AObj) =
      x
      .fuse(
          _.string_('f),
          _.int    ('g))
        .as('fg)
          .using { (f, g) =>
            f.map(_.underscore(g.str)) }

    tmp8(Default15m)
      .check(aobj(
          cls('h.boolean, 'fg.string_))(
          obj('h -> true) ))

    tmp8(Default15p)
      .check(aobj(
          cls('h.boolean, 'fg.string_))(
          obj('h -> true, 'fg -> "foo_1") ))

    tmp8(aobj(
        cls('f.string_, 'g.int ))(
        obj(            'g -> 1)))
      .dataError[vldt._Error.ObjCantBeEmpty.type]

    // ===========================================================================
    // fuse

    Default06.fuse(_.string('f1), _.string('f2)).as('f).using(_ colon _).check(bobj('g -> 1, 'f -> "foo:foo"))

    Default06
      .fuse(
          _.string('f1),
          _.string('f2))
        .as('h)
          .using { (f1, f2) =>
            f1.colon(f2.toUpperCase).reverse }
        .check(
            bobj('g -> 1, 'h -> "OOF:oof") )

    // ---------------------------------------------------------------------------
    bobj('f -> "foo", 'g -> 1, 'h -> bobj('a -> "foo", 'A -> "FOO"))
      .fuse(
          _.string      ('f),
          _.typed[Foo]('h))
        .as('i)
          .using { (f, h) => f.colon(h.A).reverse }
        .check(
            bobj('g -> 1, 'i -> "OOF:oof"))

    Default06
      .fuse(
          _.string(_.firstKey),
          _.string("f2")
          )
        .as('h)
          .using { (f1, f2) =>
            f1.colon(f2.toUpperCase).reverse }
        .check(
            bobj('g -> 1, 'h -> "OOF:oof") )

    // ===========================================================================
    // fuse baz1

    bobj('f -> "foo", 'g -> 1, 'h -> bobj('a -> "foo", 'A -> "FOO", 'q -> bobj('i -> 0)))
      .fuse(
          _.string       ('f),
          _.typed[Baz1]('h))
        .as('j)
          .using { (f, h) => f.colon((h.q.i == 0).toString).reverse }
        .check(
            bobj('g -> 1, 'j -> "eurt:oof") )

    // ---------------------------------------------------------------------------
    def fuseBaz2(u: AObj) =
        u .fuse(
              _.string       ('f),
              _.typed[Baz2]('h))
            .as('j)
              .using { (f, h) => f.colon((h.q.exists(_.i == 0)).toString).reverse }

      fuseBaz2(aobj(
          cls('f.string  , 'g.int , 'h   .cls('a.string  , 'A.string, 'q.cls_('i.int)  )))(
          obj('f -> "foo", 'g -> 1, 'h -> obj('a -> "foo", 'A -> "FOO"                 )) ))
        .check(
          bobj(             'g -> 1,                                                         'j -> "eslaf:oof" ) )

      fuseBaz2(aobj(
          cls('f.string  , 'g.int , 'h   .cls('a.string  , 'A.string  , 'q  .cls_('i.int ) )))(
          obj('f -> "foo", 'g -> 1, 'h -> obj('a -> "foo", 'A -> "FOO", 'q -> obj('i -> 0) )) ))
        .check(
          bobj(             'g -> 1,                                                         'j -> "eurt:oof") )

    // ---------------------------------------------------------------------------
    def fuseBaz3(u: AObj) =
        u .fuse(
              _.string       ('f),
              _.typed[Baz3]('h))
            .as('j)
              .using { (f, h) => f.colon((h.q.map(_.i).sum).toString).reverse }
    // ---------------------------------------------------------------------------
    aobj(
          cls('f.string  , 'g.int , 'h   .cls('a.string  , 'A.string  , 'q  .clss('i.int ) )))(
          obj('f -> "foo", 'g -> 1, 'h -> obj('a -> "foo", 'A -> "FOO", 'q -> Seq(obj('i -> 1), obj('i -> 2) ))) )
        .fuse(
              _.string       ('f),
              _.typed[Baz3]('h))
            .as('j)
              .using { (f, h) => f.colon((h.q.map(_.i).sum).toString).reverse }
        .check(
          bobj(             'g -> 1,                                                                              'j -> "3:oof"))

    // ---------------------------------------------------------------------------
    def fuseBaz4k(u: AObj) =
        u .fuse(
              _.string     ('f),
              _.typed[Baz4]('h))
            .as('j)
              .using { (f, h) => f.colon((h.q.toSeq.flatMap(_.map(_.i)).sum).toString).reverse }

      fuseBaz4k(aobj(
          cls('f.string  , 'g.int , 'h   .cls('a.string  , 'A.string, 'q.clss_('i.int)  )))(
          obj('f -> "foo", 'g -> 1, 'h -> obj('a -> "foo", 'A -> "FOO"                  )) ))
          //bobj('f -> "foo", 'g -> 1, 'h -> bobj('a -> "foo", 'A -> "FOO"                                         )))
        .check(
          bobj(             'g -> 1,                                                                              'j -> "0:oof") )

      fuseBaz4k(aobj(
          cls('f.string  , 'g.int , 'h   .cls('a.string  , 'A.string  , 'q  .clss_('i.int ) )))(
          obj('f -> "foo", 'g -> 1, 'h -> obj('a -> "foo", 'A -> "FOO", 'q -> Some(Seq(obj('i -> 1), obj('i -> 2)) ))) ))
          //bobj('f -> "foo", 'g -> 1, 'h -> bobj('a -> "foo", 'A -> "FOO", 'q -> Seq(bobj('i -> 1), bobj('i -> 2)))))
        .check(
          bobj(             'g -> 1,                                                                              'j -> "3:oof"))



      // ===========================================================================
      Default01
        .fuse(
            _.string('f),
            _.int   ('g))
          .as('fg)
            .using { (f, g) =>
              f.underscore(g.str) }
          .check(bobj('fg -> "foo_1"))

      Default15p
        .fuse(
            _.string_('f),
            _.int    ('g))
          .as('fg)
            .using { (f, g) =>
              f.map(_.underscore(g.str)).getOrElse("-") }
          .check(bobj('h -> true, 'fg -> "foo_1"))

      // ---------------------------------------------------------------------------
      Default15p
        .fuse(
            _.string_('f),
            _.int    ('g))
          .as('fg)
            .using { (f, g) =>
              f.map(_.underscore(g.str)).getOrElse(".") }
          .check(bobj('h -> true, 'fg -> "foo_1"))

      Default15m
        .fuse(
            _.string_('f),
            _.int    ('g))
          .as('fg)
            .using { (f, g) =>
              f.map(_.underscore(g.str)).getOrElse(".") }
          .check(bobj('h -> true, 'fg -> "."))

      // ---------------------------------------------------------------------------
      Default03
        .fuse(_.string('p |> 'f), _.boolean('z))
          .as('p |> 'fz)
            .using(_ colon _)
        .check(bobj('p -> bobj('g -> 1, 'fz -> "foo:true")))
  }

}

// ===========================================================================
