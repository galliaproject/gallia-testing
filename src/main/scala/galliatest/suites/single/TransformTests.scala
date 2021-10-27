package galliatest.suites.single

import gallia._
import aptus._

// ===========================================================================
object TransformTest extends gallia.testing.Suite {

  // ===========================================================================
  override def test() {
import TestDataO._  
import vldt.ErrorId._
    Default01.transform(_.string('f      )).using(_.replace("foo", "oof")).check(bobj('f -> "oof", 'g -> 1))
    Default01.transform(_.string('f ~> 'F)).using(_.replace("foo", "oof")).check(bobj('F -> "oof", 'g -> 1))

    Default06.transform(_.string('f1 ~> 'F1, 'f2)).using(_.replace("foo", "oof")).check(bobj('F1 -> "oof", 'f2 -> "oof", 'g -> 1))
    Default03.transform(_.string('p |> 'f ~> 'F) ).using(_.replace("foo", "oof")).check(bobj('p -> bobj('F -> "oof", 'g -> 1), 'z -> true))

    Default03.transform(_.obj('p)).using(
        _.rename('f ~> 'F))
      .check(bobj('p -> bobj('F -> "foo", 'g -> 1), 'z -> true))


    Default03.transform(_.obj('p)).using { _.remove('f, 'g)  }.metaError(NoFieldsLeft)
//    if (false) Default03.transform(_.obj('p)).using(identity) // FIXME: t200727153225

    Default03   .transform(_.obj('p)).using { _.remove('f)  }.check(bobj('p -> Default00, 'z -> true))

    Default01   .transform(_.string ('f)).using { _      .toUpperCase  }.check(bobj('f -> "FOO", 'g -> 1))
    Default02   .transform(_.strings('f)).using { _.map(_.toUpperCase) }.check(bobj('f -> Seq("FOO1", "FOO2"), 'g -> 1))

    Default01   .transform(_.string ('f)).using { _.in.noneIf(_.startsWith("z")).map(_.toUpperCase)  }.check(
        aobj(
            cls('f.string_ , 'g.int))(
            obj('f -> "FOO", 'g -> 1) ))
    Default01   .transform(_.string ('f)).using { _.in.noneIf(_.startsWith("f")).map(_.toUpperCase)  }.check(
        aobj(
            cls('f.string_ , 'g.int))(
            obj(             'g -> 1) ))

    // ---------------------------------------------------------------------------
    Default01   .transform(_.string('f ~> 'g)).using(_.toUpperCase).metaError(FieldAlreadyExists)

    // ---------------------------------------------------------------------------
    Default01.transform(_.string('f)).using(_.size)                          .check(bobj('f -> 3,                  'g -> 1))
    Default01.transform(_.string('f)).using(_.map(_.str))                    .check(bobj('f -> Seq("f", "o", "o"), 'g -> 1))
    Default01.transform(_.string('f)).using(s => Seq(s.head.str, s.last.str)).check(bobj('f -> Seq("f",      "o"), 'g -> 1))

    Default01.transform(_.string('f)).using(_.in.noneIf(_ => true)).check(aobj(
      cls('f.string_ , 'g.int))(
      obj(             'g -> 1)        ))

    // can't, wouldn't know about f/F at metatime: Default01   .transform(_.string('f)).using { s => /*a*/bobj('f -> s, 'F -> s.toUpperCase()) }.check()
    Default04   .transform(_.objz('p)).using { _.remove('f)  }.check(bobj('p -> Seq(Default00, Default00b), 'z -> true))


    // ===========================================================================
    Default03p.transform(_.obj('p)).using { _.remove('f)  }.check(aobj(
      cls('p   .cls_('g.int ), 'z.boolean))(
      obj('p -> obj ('g -> 1), 'z -> true) ) )

    Default03m.transform(_.obj('p)).using { _.remove('f)  }.check(aobj(
      cls('p   .cls_('g.int ), 'z.boolean))(
      obj(                     'z -> true) ) )




//disallowed:
//    Default03bMissing.transform(_.obj_('p)).using { _.map(_.remove('f)).getOrElse(???)  }.check(aobj(
//      cls('p   .cls_('g.int ), 'z.boolean),
//      obj(                     'z -> true) ) )

      //if (false) Default03.transform(_.obj('p)).using { _.forceString('f).size }.check() // FIXME: calls run... -> should use squash nows

    //Default03.transform(_.obj('p)).using { _.forceString('f).size }
      Default03.transform(_.obj('p)).using { _.retain('f).transform(_.string('f)).using(_.size) }/*.check(???)*/ // TODO: then unest
      //Default03.transform(_.string('p |> 'f)).using(_.size)

      Default03.transform(_.obj('p)).using { _.string('f).mapV(_.size) }.check(bobj('p -> 3, 'z -> true))


      // ===========================================================================
      Default02.transform(_.strings('f)).using(_.filterNot(_ == "foo2")).check(bobj('f -> Seq("foo1"), 'g -> 1))
      // 200721121201 - good whatever
      // Default02.filterItems('f, "foo2")

    // ===========================================================================
    val _RenamedDefault03  = bobj('p -> bobj('F -> "foo", 'g -> 1) , 'z -> true)

        val _RenamedDefault03p = aobj(
          cls('p   .cls_('F.string, 'g.int   ), 'z.boolean))(
          obj('p -> obj ('F -> "foo", 'g -> 1), 'z -> true) )

        val _RenamedDefault03m = aobj(
          cls('p   .cls_('F.string, 'g.int   ), 'z.boolean))(
          obj(                                  'z -> true) )

      // ---------------------------------------------------------------------------
      val _RenamedDefault04 = bobj('p -> Seq(bobj('F -> "foo", 'g -> 1), bobj('F -> "foo2", 'g -> 2)), 'z -> true)

        val _RenamedDefault04Present = aobj(
          cls('p   .clss_  ('F.string,   'g.int),                                'z.boolean))(
          obj('p -> Seq(obj('F -> "foo", 'g -> 1), obj('F -> "foo2", 'g -> 2)) , 'z -> true) )

        val _RenamedDefault04Missing = aobj(
          cls('p   .clss_('F.string, 'g.int   ), 'z.boolean))(
          obj(                                   'z -> true) )

        // ---------------------------------------------------------------------------
        Default03 .transformObject('p).using(_.rename('f ~> 'F)).check(_RenamedDefault03)
        Default03p.transformObject('p).using(_.rename('f ~> 'F)).check(_RenamedDefault03p)
        Default03m.transformObject('p).using(_.rename('f ~> 'F)).check(_RenamedDefault03m)

        Default04 .transformObjects('p).using(_.rename('f ~> 'F)).check(_RenamedDefault04       )
        Default04p.transformObjects('p).using(_.rename('f ~> 'F)).check(_RenamedDefault04Present)
        Default04m.transformObjects('p).using(_.rename('f ~> 'F)).check(_RenamedDefault04Missing)

    // ===========================================================================
    // combinations

    Default01 .transform(_.string  ('f)).using(_            .toUpperCase  ).check(                              bobj('f ->     "FOO"          , 'g -> 1))
    Default02 .transform(_.strings ('f)).using(      _.map(_.toUpperCase) ).check(                              bobj('f -> Seq("FOO1", "FOO2"), 'g -> 1))
    Default13p.transform(_.string_ ('f)).using(      _.map(_.toUpperCase) ).check(aobj(cls('f.string_ , 'g.int))(obj('f ->     "FOO"          , 'g -> 1)))
    Default13m.transform(_.string_ ('f)).using(      _.map(_.toUpperCase) ).check(aobj(cls('f.string_ , 'g.int))(obj(                           'g -> 1)))
    Default14p.transform(_.strings_('f)).using(_.map(_.map(_.toUpperCase))).check(aobj(cls('f.strings_, 'g.int))(obj('f -> Seq("FOO1", "FOO2"), 'g -> 1)))
    Default14m.transform(_.strings_('f)).using(_.map(_.map(_.toUpperCase))).check(aobj(cls('f.strings_, 'g.int))(obj(                           'g -> 1)))

    // ---------------------------------------------------------------------------
    // ignore container
    Default01 .transform(_.stringx('f)).using(_            .toUpperCase  ).check(                              bobj('f ->     "FOO"          , 'g -> 1))
    Default02 .transform(_.stringx('f)).using(_            .toUpperCase  ).check(                              bobj('f -> Seq("FOO1", "FOO2"), 'g -> 1))
    Default13p.transform(_.stringx('f)).using(_            .toUpperCase  ).check(aobj(cls('f.string_ , 'g.int))(obj('f ->     "FOO"          , 'g -> 1)))
    Default13m.transform(_.stringx('f)).using(_            .toUpperCase  ).check(aobj(cls('f.string_ , 'g.int))(obj(                           'g -> 1)))
    Default14p.transform(_.stringx('f)).using(_            .toUpperCase  ).check(aobj(cls('f.strings_, 'g.int))(obj('f -> Seq("FOO1", "FOO2"), 'g -> 1)))
    Default14m.transform(_.stringx('f)).using(_            .toUpperCase  ).check(aobj(cls('f.strings_, 'g.int))(obj(                           'g -> 1)))

    // ---------------------------------------------------------------------------
    // more whatever
    Default01 .transform('f).using(_ + "X").check(                              bobj('f ->     "fooX"           , 'g -> 1))
    Default02 .transform('f).using(_ + "X").check(                              bobj('f -> Seq("foo1X", "foo2X"), 'g -> 1))
    Default13p.transform('f).using(_ + "X").check(aobj(cls('f.string_ , 'g.int))(obj('f ->     "fooX"           , 'g -> 1)))
    Default13m.transform('f).using(_ + "X").check(aobj(cls('f.string_ , 'g.int))(obj(                             'g -> 1)))
    Default14p.transform('f).using(_ + "X").check(aobj(cls('f.strings_, 'g.int))(obj('f -> Seq("foo1X", "foo2X"), 'g -> 1)))
    Default14m.transform('f).using(_ + "X").check(aobj(cls('f.strings_, 'g.int))(obj(                             'g -> 1)))
  }

}

// ===========================================================================
