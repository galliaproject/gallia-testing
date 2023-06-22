package galliatesting0
package suites

import aptus._
import gallia._

// ===========================================================================
/** obg9 is codename for memory-optimized Obj counterpart (not in use yet) - good for dense data */
object Obg9Test extends gallia.testing.Suite with gallia.testing.More {
  import TestDataO._
  import Obg9TestData._

  // ---------------------------------------------------------------------------
  override def test(): Unit = {
    import gallia.atoms.obg9.Obg9Contexts._

    RemoveContiguousCtx.from(totalSize = 2)(fromInclusive = 0, toInclusive = 1)
      .assert(_ == RemoveContiguousCtx(Range(0, 0, 0), Range(2, 0, 0), 0))

    RemoveContiguousCtx.from(totalSize = 7)(fromInclusive = 0, toInclusive = 3)
      .assert(_ == RemoveContiguousCtx(Range(0, 0, 0), Range(4, 0, 3), 3))

    RemoveContiguousCtx.from(totalSize = 7)(fromInclusive = 2, toInclusive = 6)
      .assert(_ == RemoveContiguousCtx(Range(0, 0, 2), Range(7, 2, 0), 2))

    RemoveContiguousCtx.from(totalSize = 7)(fromInclusive = 2, toInclusive = 3)
      .assert(_ == RemoveContiguousCtx(Range(0, 0, 2), Range(4, 2, 3), 5))

    // ===========================================================================
  // atoms.obg9.hack.temporarily {
    bobj("f" -> "foo", "g" -> 1)
      .transform(_.string("f")).using(_.toUpperCase)
      ._assert(bobj("f" -> "FOO", "g" -> 1))

    j .read()
      .transformString("contact" |> "addresses" |> "street_name")
      .using(_.replace("Avenue", "Ave"))
      .formatCompactJson
      .assert(_ == J, _ -> J)

    j .read()
      .transform(_.string("contact" |> "addresses" |> "street_name"))
      .using(_.replace("Avenue", "Ave"))
      .formatCompactJson
      .assert(_ == J, _ -> J)

    // ---------------------------------------------------------------------------
    Default03p.transform(_.string("p" |> "f")).using(_.toUpperCase)._assert(_Default03p)
    Default03m.transform(_.string("p" |> "f")).using(_.toUpperCase)._assert( Default03m)

    Default13p.transform(_.string_ ("f")).using(_.map(      _.toUpperCase).getOrElse("-")) ._assert(bobj("f" -> "FOO", "g" -> 1))
    Default13m.transform(_.string_ ("f")).using(_.map(      _.toUpperCase).getOrElse("-")) ._assert(bobj("f" -> "-"  , "g" -> 1))

    Default14p.transform(_.strings_ ("f")).using(_.map(      _.map(_.toUpperCase)).getOrElse(Seq("-", "-"))) ._assert(aobj("f".strings, "g".int)(obj("f" -> Seq("FOO1", "FOO2"), "g" -> 1)))
    Default14m.transform(_.strings_ ("f")).using(_.map(      _.map(_.toUpperCase)).getOrElse(Seq("-", "-"))) ._assert(aobj("f".strings, "g".int)(obj("f" -> Seq("-",    "-"),    "g" -> 1)))

    // ---------------------------------------------------------------------------
    Default06a.fuse(_.string("f1"), _.string("f2")).as("f").using { _.length + _.length }._assert(bobj("g" -> 1, "f" -> 8))
    Default01 .fission(_.string("f")).as("f2", "f3").using { f => (f.append("2"), f.append("3")) }._assert(bobj("g" -> 1, "f2" -> "foo2", "f3" -> "foo3"))

    // ===========================================================================
    atoms.obg9.hack.temporarily {
      j .read()
        .transform(_.string("contact" |> "addresses" |> "street_name"))
        .using(_.replace("Avenue", "Ave"))
        .formatCompactJson
        .assert(_ == J)

      // ---------------------------------------------------------------------------
      Default01.rename("f" ~> "f2")._assert(bobj("f2" -> "foo", "g" -> 1))

      Default01.rename(_.toUpperCase)._assert(bobj("F" -> "foo", "G" -> 1))

      Default01.add    ("f2" -> "foo2")._assert(bobj("f" -> "foo" , "g" -> 1, "f2" -> "foo2"))
      Default01.replace("f"  -> "foo2")._assert(bobj("f" -> "foo2" , "g" -> 1))

      Default01.remove("f")._assert(bobj("g" -> 1))
      Default01.remove("g")._assert(bobj("f" -> "foo"))

      Default06.remove("f1")      ._assert(bobj(               "f2" -> "foo", "g" -> 1))
      Default06.remove("f2")      ._assert(bobj("f1" -> "foo",                "g" -> 1))
      Default06.remove("g")       ._assert(bobj("f1" -> "foo", "f2" -> "foo"))
      Default06.remove("f1", "f2")._assert(bobj(                              "g" -> 1))
      Default06.remove("f1", "g") ._assert(bobj(               "f2" -> "foo"))

      Default06.retain("f1")      ._assert(bobj("f1" -> "foo"))
      Default06.retain("f2")      ._assert(bobj(               "f2" -> "foo"))
      Default06.retain("g")       ._assert(bobj(                              "g" -> 1))
      Default06.retain("f1", "f2")._assert(bobj("f1" -> "foo", "f2" -> "foo"))
      Default06.retain("f1", "g") ._assert(bobj("f1" -> "foo",                "g" -> 1))

      Default01.add    ("f2" -> "foo2", "f3" -> "foo3")._assert(bobj("f" -> "foo", "g" -> 1, "f2" -> "foo2", "f3" -> "foo3"))
      Default01.replace("f"  -> "foo2", "g"  -> 2)     ._assert(bobj("f" -> "foo2", "g" -> 2))

      Default01.reorderKeys           (_.reverse)._assert(                         bobj("g" -> 1, "f" -> "foo"))
      Default03.reorderKeysRecursively(_.reverse)._assert(bobj("z" -> true, "p" -> bobj("g" -> 1, "f" -> "foo")))

      // ---------------------------------------------------------------------------
      // nesting

      Default03 .remove("p" |> "f")      ._assert(bobj("p" -> Default00, "z" -> true))
      Default03b.remove("p" |> "f1", "z")._assert(bobj("p" -> bobj("f2" -> 2), "f3" -> 3))

      Default03b.retain(                      "f3",          "z")._assert(bobj(                                   "f3" -> 3, "z" -> true))
      Default03b.retain("p",                                 "z")._assert(bobj("p" -> bobj("f1" -> 1, "f2" -> 2),            "z" -> true))
      Default03b.retain("p" |> "f1",                         "z")._assert(bobj("p" -> bobj("f1" -> 1),                       "z" -> true))

      // ===========================================================================
      Default01 .transform(_.string  ("f")).using(_            .toUpperCase)  ._assert(_Default01)
      Default02 .transform(_.strings ("f")).using(      _.map(_.toUpperCase)) ._assert(_Default02)
      Default13p.transform(_.string_ ("f")).using(_.map(      _.toUpperCase)) ._assert(_Default13p)
      Default13m.transform(_.string_ ("f")).using(_.map(      _.toUpperCase)) ._assert( Default13m)
      Default14p.transform(_.strings_("f")).using(_.map(_.map(_.toUpperCase)))._assert(_Default14p)
      Default14m.transform(_.strings_("f")).using(_.map(_.map(_.toUpperCase)))._assert( Default14m)
      Default13p.transform(_.string_ ("f")).using(_.map(      _.toUpperCase).getOrElse("-")) ._assert(bobj("f" -> "FOO", "g" -> 1))
      Default13m.transform(_.string_ ("f")).using(_.map(      _.toUpperCase).getOrElse("-")) ._assert(bobj("f" -> "-"  , "g" -> 1))

      Default01 .transform(_.string  ("f")).using(_.in.noneIf(_ == "foo"))._assert(aobj(cls("f".string_, "g".int))(obj(             "g" -> 1)))
      Default01 .transform(_.string  ("f")).using(_.in.noneIf(_ == "FOO"))._assert(aobj(cls("f".string_, "g".int))(obj("f" -> "foo", "g" -> 1)))

      // nesting
      Default03 .transform(_.string("p" |> "f")).using(_.toUpperCase)._assert(_Default03)
      Default03p.transform(_.string("p" |> "f")).using(_.toUpperCase)._assert(_Default03p)
      Default03m.transform(_.string("p" |> "f")).using(_.toUpperCase)._assert( Default03m)

      Default04 .transform(_.string("p" |> "f")).using(_.toUpperCase)._assert(_Default04)
      Default04p.transform(_.string("p" |> "f")).using(_.toUpperCase)._assert(_Default04p)
      Default04m.transform(_.string("p" |> "f")).using(_.toUpperCase)._assert( Default04m)

      // renaming
      Default01.transform(_.string  ("f" ~> "F")).using(_.toUpperCase)  ._assert(bobj("F" -> "FOO" , "g" -> 1))

      // nesting+renaming
      Default03 .transform(_.string("p" |> "f" ~> "F")).using(_.toUpperCase)._assert(bobj("p" -> bobj("F" -> "FOO" , "g" -> 1), "z" -> true))

      // ===========================================================================
      Default01.generate("f2").from(_.string("f")).using(_.toUpperCase)._assert(bobj("f" -> "foo", "g" -> 1, "f2" -> "FOO"))
      Default01.generate("f2").from(_.string("f")).using(_.in.noneIf(_ == "oof").map(_.toUpperCase))._assert(aobj("f".string, "g".int, "f2".string_)(obj("f" -> "foo", "g" -> 1, "f2" -> "FOO")))
      Default01.generate("f2").from(_.string("f")).using(_.in.noneIf(_ == "foo").map(_.toUpperCase))._assert(aobj("f".string, "g".int, "f2".string_)(obj("f" -> "foo", "g" -> 1)))

      Default02 .generate("f2").from(_.strings("f")).using(_.map(_.toUpperCase))._assert(aobj("f".strings, "g".int, "f2".strings)(obj("f" -> Seq("foo1", "foo2"), "g" -> 1, "f2" -> Seq("FOO1", "FOO2"))))
      Default13p.generate("f2").from(_.string_("f")).using(_.map(_.toUpperCase))._assert(aobj("f".string_, "g".int, "f2".string_)(obj("f" -> "foo", "g" -> 1, "f2" -> "FOO")))
      Default13m.generate("f2").from(_.string_("f")).using(_.map(_.toUpperCase))._assert(aobj("f".string_, "g".int, "f2".string_)(obj(              "g" -> 1              )))

      Default14p.generate("f2").from(_.strings_("f")).using(_.map(_.map(_.toUpperCase)))._assert(aobj("f".strings_, "g".int, "f2".strings_)(obj("f" -> Seq("foo1", "foo2"), "g" -> 1, "f2" -> Seq("FOO1", "FOO2"))))
      Default14m.generate("f2").from(_.strings_("f")).using(_.map(_.map(_.toUpperCase)))._assert(aobj("f".strings_, "g".int, "f2".strings_)(obj(                           "g" -> 1)))

      Default13p.generate("f2").from(_.string_("f")).using(_.map(_.toUpperCase).getOrElse("-"))._assert(aobj("f".string_, "g".int, "f2".string)(obj("f" -> "foo", "g" -> 1, "f2" -> "FOO")))
      Default13m.generate("f2").from(_.string_("f")).using(_.map(_.toUpperCase).getOrElse("-"))._assert(aobj("f".string_, "g".int, "f2".string)(obj(             "g" -> 1, "f2" -> "-")))

      Default14p.transform(_.strings_ ("f")).using(_.map(      _.map(_.toUpperCase)).getOrElse(Seq("-", "-"))) ._assert(aobj("f".strings, "g".int)(obj("f" -> Seq("FOO1", "FOO2"), "g" -> 1)))
      Default14m.transform(_.strings_ ("f")).using(_.map(      _.map(_.toUpperCase)).getOrElse(Seq("-", "-"))) ._assert(aobj("f".strings, "g".int)(obj("f" -> Seq("-",    "-"),    "g" -> 1)))

      Default03.generate("f2").from(_.string("p" |> "f")).using(_.toUpperCase)._assert(bobj("p" -> Default01 , "z" -> true, "f2" -> "FOO"))

      Default06a.generate("f3").from(_.string("f1"), _.string("f2")).using { _.length + _.length }._assert(bobj("f1" -> "foo1", "f2" -> "foo2", "g" -> 1, "f3" -> 8))
      Default01 .generate("f2", "f3").from(_.string("f")).using { f => (f.append("2"), f.append("3")) }._assert(bobj("f" -> "foo", "g" -> 1, "f2" -> "foo2", "f3" -> "foo3"))

      if (false) // TODO: t220418154807: paths
        Default03
          .generate("p" |> "g2")
          .from(_.int("p" |> "g"))
          .using(_ + 1)
          ._assert(bobj("p" -> bobj("f" -> "foo", "g" -> 1, "g2" -> 2), "z" -> true))

      Default01.removeIfValueFor(_.string("f")).is("FOO")._assert(aobj(cls("f".string_, "g".int))(obj("f" -> "foo", "g" -> 1)))
      Default01.removeIfValueFor(_.string("f")).is("foo")._assert(aobj(cls("f".string_, "g".int))(obj(               "g" -> 1)))

      Default03.removeIfValueFor(_.string("p" |> "f")).is("FOO")._assert(aobj(cls("p".cls("f".string_, "g".int), "z".boolean))(obj("p" -> obj("f" -> "foo", "g" -> 1), "z" -> true)))
      Default03.removeIfValueFor(_.string("p" |> "f")).is("foo")._assert(aobj(cls("p".cls("f".string_, "g".int), "z".boolean))(obj("p" -> obj(              "g" -> 1), "z" -> true)))

      Default13p.setDefaultFor("f").asValue("FOO")._assert(Default01)
      Default13m.setDefaultFor("f").asValue("foo")._assert(Default01)

      Default14p.setDefaultFor("f").asValue(Seq("FOO1", "FOO2"))._assert(Default02)
      Default14m.setDefaultFor("f").asValue(Seq("foo1", "foo2"))._assert(Default02)

      aobj(cls("p".cls("f".string_, "g".int), "z".boolean))(obj("p" -> obj("g" -> 1), "z" -> true)).setDefaultFor("p" |> "f").asValue("foo")._assert(Default03)

      // fuse/fission
      Default06a.fuse(_.string("f1"), _.string("f2")).as("f").using { _.length + _.length }._assert(bobj("g" -> 1, "f" -> 8))
      Default01 .fission(_.string("f")).as("f2", "f3").using { f => (f.append("2"), f.append("3")) }._assert(bobj("g" -> 1, "f2" -> "foo2", "f3" -> "foo3"))
    }
  }

  // ===========================================================================
  object Obg9TestData {

    val j =
      """
        {
          "name": "John Smith",
          "contact": {
            "addresses": [
              { "street_name": "3 Blue Street" },
              { "street_name": "4 Red Avenue" }
            ]
          }
        }
      """

    val J = """{"name":"John Smith","contact":{"addresses":[{"street_name":"3 Blue Street"},{"street_name":"4 Red Ave"}]}}"""

    // ===========================================================================
    implicit class ic220412163227(u: HeadO) {
      @aptus.fordevonly def _assert(expected: BObj): HeadO = _assert(expected.forceAObj)

      // ---------------------------------------------------------------------------
      @aptus.fordevonly def _assert(exp: AObj): HeadO = {
        val actual = u._forceResult

        assert(actual.c == exp.c, Seq(actual.c, exp.c).joinln.newline)
        assert(actual.o == exp.o, Seq(actual.o, exp.o).joinln.newline)

        actual
      }

    }

    // ===========================================================================
    val _Default01    = bobj('f -> "FOO" , 'g -> 1)
    val _Default02    = bobj('f -> Seq("FOO1", "FOO2"), 'g -> 1)

    val _Default03  = bobj('p -> _Default01 , 'z -> true)

    val _Default13p  = aobj(cls('f.string_, 'g.int))(obj('f -> "FOO", 'g -> 1))
    val _Default13m  = aobj(cls('f.string_, 'g.int))(obj(             'g -> 1))

    val _Default14p  = aobj(cls('f.strings_, 'g.int))(obj('f -> Seq("FOO1", "FOO2"), 'g -> 1))
    val _Default14m  = aobj(cls('f.strings_, 'g.int))(obj(                           'g -> 1))

    val _Default03p = aobj(
      cls('p   .cls_('f.string  , 'g.int ), 'z.boolean))(
      obj('p -> obj ('f -> "FOO", 'g -> 1), 'z -> true) )

    val _Default01b = bobj('f -> "FOO2", 'g -> 2)
    val _Default04 = bobj('p -> Seq(_Default01, _Default01b), 'z -> true)

    val _Default04p = aobj(
      cls('p   .clss_  ('f.string,   'g.int),                                'z.boolean))(
      obj('p -> Seq(obj('f -> "FOO", 'g -> 1), obj('f -> "FOO2", 'g -> 2)) , 'z -> true) )

  }

}

// ===========================================================================

