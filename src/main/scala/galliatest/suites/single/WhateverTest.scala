package galliatest.suites.single

import gallia._

// ===========================================================================
object WhateverTest extends gallia.testing.Suite {
	import TestDataO.{Default01, Default02, Default14m, Default14p}

	// ---------------------------------------------------------------------------
  override def test() {
    bobj('v -> "foo", 'z -> true).removeIfValueFor('v).is("foo").check(aobj(cls('v.string_, 'z.boolean))(obj(             'z -> true)))
    bobj('v -> "foo", 'z -> true).removeIfValueFor('v).is("FOO").check(aobj(cls('v.string_, 'z.boolean))(obj('v -> "foo", 'z -> true)))

    bobj('v -> Seq(1, 2, 3)).toSize('v)      .check(bobj('v -> 3))
  //bobj('v -> Seq(1, 2, 3)).toSize('v ~> 'V).check(bobj('V -> 3)) // FIXME: renaming

    bobj('v -> Seq(1, 2, 3)).toSize('v)      .check(bobj('v -> 3))

  //Default14m.toSize('f).check(bobj('f -> 0, 'g -> 1)) // FIXME...
    Default14p.toSize('f).check(bobj('f -> 2, 'g -> 1))
  //Default13p.toSize('f).check(bobj('f -> 2, 'g -> 1)) // FIXME

    Default14m.transform(_.strings_('f)).using(_.map(_.size).getOrElse(0)).check(bobj('f -> 0, 'g -> 1))

    Default01.transform('f).using(_ == "foo").check(bobj('f -> true , 'g -> 1))
    Default01.transform('f).using(_ == "FOO").check(bobj('f -> false, 'g -> 1))
    Default01.transform('f).using(_ != "foo").check(bobj('f -> false, 'g -> 1))
    Default01.transform('f).using(_ != "FOO").check(bobj('f -> true , 'g -> 1))


    //bobj('v -> Seq(1, 2, 3)).transform ('v).using(x => Seq(x +  1 , x +  2 )).check(bobj('v -> Seq(  2 ,   3 ) ))
     bobj('v -> 1)            .transform ('v).using(x => Seq(x +  1 , x +  2 )).check(bobj('v -> Seq(  2 ,   3 ) ))
     bobj('v -> 1)            .transform ('v).using(x => Seq(x      , x      )).check(bobj('v -> Seq(  1 ,   1 ) ))

    // ---------------------------------------------------------------------------
    bobj('v -> 2.0).transform('v).using(_.square)   .check(bobj('v -> 4.0))
    bobj('v -> 1  ).transform('v).using(_.increment).check(bobj('v -> 2))
    bobj('v -> true).transform('v).using(_.flip)   .check(bobj('v -> false))
    bobj('v -> "foo").transform('v).using(_.toUpperCase)   .check(bobj('v -> "FOO"))
    bobj('v -> "foo").transform('v).using(s => s"|${s}|")   .check(bobj('v -> "|foo|"))

    bobj('v -> "foo").transform('v).using(_.sizeString)   .check(bobj('v -> 3))

    bobj('f -> "a,b,c", 'g -> "1,2,3").fuse('f, 'g).as('fg).using(_ + ":" + _)            .check(bobj('fg -> "a,b,c:1,2,3"))
    bobj('f -> "a,b,c", 'g -> "1,2,3").fuse('f, 'g).as('fg).using((a, b) => a + ":" + ":").check(bobj('fg -> "a,b,c::"))

    bobj('f -> 2, 'g -> 3).fuse('f, 'g).as('fg).using((a, b) => a + b).check(bobj('fg -> 5))
    bobj('f -> 2, 'g -> 3).fuse('f, 'g).as('fg).using((a, b) => a * b).check(bobj('fg -> 6))

    bobj('f -> 2, 'g -> 3).fuse('f, 'g).as('fg).using((a, b) => a + 1 + b).check(bobj('fg -> 6))

    bobj('f -> 2, 'g -> 3).fuse('f, 'g).as('fg).using((a, b) => (a + 1) > 1).check(bobj('fg -> true ))
    bobj('f -> 2, 'g -> 3).fuse('f, 'g).as('fg).using((a, b) => (a + 1) < 1).check(bobj('fg -> false))

    bobj('f -> 2, 'g -> 3).fuse('f, 'g).as('fg).using((a, b) => (a + b)    .square).check(bobj('fg -> 25.0))
    bobj('f -> 2, 'g -> 3).fuse('f, 'g).as('fg).using((a, b) => (a + b + 1).square).check(bobj('fg -> 36.0))

    // explicit
    bobj('f -> 2, 'g -> 3).fuse(           'f,             'g ).as('fg).using((a, b) => a + b).check(bobj('fg -> 5))
  //bobj('f -> 2, 'g -> 3).fuse(_.explicit('f), _.explicit('g)).as('fg).using((a, b) => a + b).check(bobj('fg -> 5)) // disallowed

    // ===========================================================================
    // transform

    bobj('v -> 1  ).transform ('v).using(x => Seq(x +  1 , x +  2 )).check(bobj('v -> Seq(  2 ,   3 ) ))
    bobj('v -> 1  ).transform ('v).using(x => Seq(x + "1", x + "2")).check(bobj('v -> Seq("11", "12") ))

    bobj('v -> 1  ).transform ('v).using(_ + 1  ).check(bobj('v -> 2  ))

    bobj('v -> "1").transform ('v).using(_ + "1").check(bobj('v -> "11"))
    bobj('v -> 1  ).transform ('v).using(_ + "1").check(bobj('v -> "11"))
    bobj('v -> 1.1).transform ('v).using(_ + "1").check(bobj('v -> "1.11"))

    bobj('v -> 1  ).transform ('v).using(_ +   1).check(bobj('v -> 2  ))
    bobj('v -> 1  ).transform ('v).using(_ + 1.1).check(bobj('v -> 2.1))

    bobj('v -> 1.1).transform ('v).using(_ + 1  ).dataError("210112140145" -> "")//test(bobj('v -> 2.1))
    bobj('v -> 1.1).transform ('v).using(_ + 1.1).check(bobj('v -> 2.2))

    bobj('v -> 1  ).transform ('v).using(_ + "1").check(bobj('v -> "11"))

    // ---------------------------------------------------------------------------
    Default01.transform ('f)            .using(_ + "bar").check(bobj('f -> "foobar", 'g -> 1))
    Default01.transform (_.string('f))  .using(_ + "bar").check(bobj('f -> "foobar", 'g -> 1))
  //Default01.transform(_.explicit('f)).using(x => x + "bar").check(bobj('f -> "foobar", 'g -> 1)) - FIXME

    Default01.transform('f).using(_.sizeString).check(bobj('f ->  3 , 'g -> 1))

    // ---------------------------------------------------------------------------
    //Default01.transform0(_.$('f)).using(x => x + 1).test__

    Default01.transform('f).using(_ == "foo").check(bobj('f -> true , 'g -> 1))
    Default01.transform('f).using(_ != "foo").check(bobj('f -> false, 'g -> 1))

    Default01.transform('g).using(_ == 1).check(bobj('f -> "foo" , 'g -> true))
    Default01.transform('g).using(_ != 1).check(bobj('f -> "foo", 'g -> false))

    //Default02.transform('f).using(_.sizeString).test__ //FIXME

    Default01.transform(_.string ('f)).using(x => x.size + 1).check(bobj('f -> 4, 'g -> 1))
    Default02.transform(_.strings('f)).using(x => x.size + 1).check(bobj('f -> 3, 'g -> 1))

    // ===========================================================================

    Default01
      .generate('f2)
        .from('f) // whatever
          .using(_ + "_2")
        .check(bobj('f -> "foo", 'g -> 1, 'f2 -> "foo_2"))

    Default01
      .generate('g2)
        .from('g)
          .using(_ + 3)
        .check(bobj('f -> "foo", 'g -> 1, 'g2 -> 4))

    // ===========================================================================
    // fuse

    bobj('f -> "a,b,c", 'g -> "1,2,3")
        .fuse(_.string('f), _.string('g)).as('fg).using((f, g) => Seq(f, g))
      .check(bobj('fg -> Seq("a,b,c", "1,2,3")))

    bobj('f -> "a,b,c", 'g -> "1,2,3")
        .fuse('f, 'g).as('fg).using((f, g) => Seq(f, g))
      .check(bobj('fg -> Seq("a,b,c", "1,2,3")))

    bobj('f -> "a,b,c", 'g -> "1,2,3")
        .fuse('f, 'g).as('fg).using((f, g) => f + g)
      .check(bobj('fg -> "a,b,c1,2,3"))

    bobj('f -> "a,b,c", 'g -> "1,2,3")
        .fuse('f, 'g).as('fg).using(_ + ":" + _)
      .check(bobj('fg -> "a,b,c:1,2,3"))

    bobj('f -> "a,b,c", 'g -> "1,2,3")
        .fuse('f, 'g).as('fg).using { (v1, v2) => Seq(v1 + ":" + v2, v1 + ";" + v2) }
      .check(bobj('fg -> Seq("a,b,c:1,2,3", "a,b,c;1,2,3")))

if (false)
    aobj(cls('f.string_, 'g.string_))(obj('f -> "a,b,c", 'g -> "1,2,3"))
        .fuse('f, 'g).as('fg).using { (v1, v2) => Seq(v1 + ":" + v2, v1 + ";" + v2) }
      .check(bobj('fg -> Seq("a,b,c:1,2,3", "a,b,c;1,2,3")))
  }


}

// ===========================================================================
