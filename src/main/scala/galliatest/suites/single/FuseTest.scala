package galliatest.suites.single

import aptus._
import gallia._

// ===========================================================================
object FuseTest extends gallia.testing.Suite {
	import TestDataO._  
	import TestMeta ._

  // ===========================================================================
  override def test() {

	  Default06a
  	    .fuse('f1, 'f2)
  	      .as('f12)
  	        .using(_ + _)
  	    .check(bobj('g -> 1, 'f12 -> "foo1foo2"))	    
  
  	  Default06a
  	    .fuse('f1, 'f2)
  	      .as('f12)
  	        .using(_.sizeString + _.sizeString)
  	    .check(bobj('g -> 1, 'f12 -> 8))
  	    
  	  Default06a
  	    .fuse('f1, 'f2)
  	      .as('f12)
  	        .using(_.toString + _.toString)
  	    .check(bobj('g -> 1, 'f12 -> "foo1foo2"))	    
	    
	  Default06a.add('f3 -> "foo3")
  	    .fuse('f1, 'f2, 'f3)
  	      .as('f123)
  	        .using(_ + _ + _)
  	    .check(bobj('g -> 1, 'f123 -> "foo1foo2foo3"))	      
  	    
	  // ===========================================================================
    fuseTest1(Default15m)
      .check(aobj(
          cls('h.boolean, 'fg.string_))(
          obj('h -> true) ))

    fuseTest1(Default15p)
      .check(aobj(
          cls('h.boolean, 'fg.string_))(
          obj('h -> true, 'fg -> "foo_1") ))

    fuseTest1(aobj(
        cls('f.string_, 'g.int ))(
        obj(            'g -> 1)))
      .dataError[vldt._Error.ObjCantBeEmpty.type]

    // ===========================================================================
    fuseTest2(Default15m)
      .check(aobj(
          cls('h.boolean, 'fg.string_))(
          obj('h -> true) ))

    fuseTest2(Default15p)
      .check(aobj(
          cls('h.boolean, 'fg.string_))(
          obj('h -> true, 'fg -> "foo_1") ))

    fuseTest2(aobj(
        cls('f.string_, 'g.int ))(
        obj(            'g -> 1)))
      .dataError[vldt._Error.ObjCantBeEmpty.type]

    // ===========================================================================
    Default06
      .fuse(_.string('f1), _.string('f2))
        .as('f).using(_ colon _)
      .check(bobj('g -> 1, 'f -> "foo:foo"))

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
          _.string("f2") )
        .as('h)
          .using { (f1, f2) =>
            f1.colon(f2.toUpperCase).reverse }
        .check(
            bobj('g -> 1, 'h -> "OOF:oof") )

    // ===========================================================================
    // Baz1

    bobj('f -> "foo", 'g -> 1, 'h -> bobj('a -> "foo", 'A -> "FOO", 'q -> bobj('i -> 0)))
      .fuse(
          _.string       ('f),
          _.typed[Baz1]('h))
        .as('j)
          .using { (f, h) => f.colon((h.q.i == 0).toString).reverse }
        .check(
            bobj('g -> 1, 'j -> "eurt:oof") )

    // ---------------------------------------------------------------------------
    // Baz2
            
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
    // Baz3
        
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
    // Baz4
          
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

	// ===========================================================================
  private def fuseTest1(x: AObj): HeadU =
    x
      .fuse(
          _.string_('f),
          _.int    ('g))
        .as('fg)
          .using { (f, g) =>
            f.map(_.underscore(g.str)) }
  // ---------------------------------------------------------------------------
  private def fuseTest2(x: AObj) =
    x
    .fuse(
        _.string_('f),
        _.int    ('g))
      .as('fg)
        .using { (f, g) =>
          f.map(_.underscore(g.str)) }

  // ---------------------------------------------------------------------------
  private def fuseBaz2(u: AObj) =
    u .fuse(
          _.string     ('f),
          _.typed[Baz2]('h))
        .as('j)
          .using { (f, h) => f.colon((h.q.exists(_.i == 0)).toString).reverse }

  // ---------------------------------------------------------------------------
  private def fuseBaz3(u: AObj) =
    u .fuse(
          _.string       ('f),
          _.typed[Baz3]('h))
        .as('j)
          .using { (f, h) => f.colon((h.q.map(_.i).sum).toString).reverse }
  
  // ---------------------------------------------------------------------------
  private def fuseBaz4k(u: AObj) =
    u .fuse(
          _.string     ('f),
          _.typed[Baz4]('h))
        .as('j)
          .using { (f, h) => f.colon((h.q.toSeq.flatMap(_.map(_.i)).sum).toString).reverse }
    
}

// ===========================================================================
