package galliatesting0
package suites

import scala.reflect.runtime.universe.WeakTypeTag
import aptus._
import gallia._
import gallia.meta.SubInfo._

// ===========================================================================
object UnionTypeTest extends gallia.testing.Suite with gallia.testing.More { import sandbox._

	// ---------------------------------------------------------------------------
	implicit class Float_(u: Float) { def incrementFloat: Float = u + 1 }

  // ===========================================================================
  override def test() { TestDataO.Default01.noop(_.identity)

    {
      val c = cls("f".requiredUnion(string, int))

                """{"f": 3}""".read(_.schema(c))._forceResult.forceInt("f").assert(_ == 3)             // test gson -> gallia only
      aobj(c)(obj("f" -> 3)).formatCompactJson                             .assert(_ == """{"f":3}""") // test gallia -> gson only
    }

    // ---------------------------------------------------------------------------
    """{"f": "foo", "g": 1.1}""".read()._assert2(bobj("f" -> "foo", "g" -> 1.1))
    """{"f": "foo", "g": 1  }""".read()._assert2(bobj("f" -> "foo", "g" -> 1))

    // ---------------------------------------------------------------------------
                bobj("f" -> "foo", "g" -> 1) .increment("g")       ._assert2(            bobj("f" -> "foo", "g" -> 2))
    bobj("p" -> bobj("f" -> "foo", "g" -> 1)).increment("p" |> "g")._assert2(bobj("p" -> bobj("f" -> "foo", "g" -> 2)))

    // ---------------------------------------------------------------------------
    aobj(        cls("f".string, "g".int)) (           obj("f" -> "foo", "g" -> 1)) .increment("g")       ._assert2(aobj(        cls("f".string, "g".int)) (           obj("f" -> "foo", "g" -> 2)))
    aobj(cls("p".cls("f".string, "g".int)))(obj("p" -> obj("f" -> "foo", "g" -> 1))).increment("p" |> "g")._assert2(aobj(cls("p".cls("f".string, "g".int)))(obj("p" -> obj("f" -> "foo", "g" -> 2))))

    // ===========================================================================
    {
      val ca = cls("f".int)
      val cb = cls("f".string)

      unionTest2(ca)("""{"f":  3   }""",  4   ) { _.increment("f") }
      unionTest2(ca)("""{"f":  3   }""",  4   ) { _.transform(_.int   ("f")).using(_ + 1) }
      unionTest2(cb)("""{"f": "foo"}""", "FOO") { _.transform(_.string("f")).using(_.toUpperCase) }
    }

    // ---------------------------------------------------------------------------
    val c1 = cls("f".requiredUnion(string, int))
   		unionTest1(c1)("""{"f": "foo"}""", "FOO")
      unionTest1(c1)("""{"f":     3}""", 3)

    // ---------------------------------------------------------------------------
    {
      val c = cls("f".requiredUnion(string, single(_._Float)))

      unionTest1(c)("""{"f": "foo"}""", "FOO")
      unionTest1(c)("""{"f":   3.3}""",  3.3.toFloat)
      unionTest2(c)("""{"f": "foo"}""", "foo")                  { _.transform(_.float("f")).using(_.incrementFloat) }
      unionTest3(aobj(c)(obj("f" -> 3.3.toFloat)))(4.3.toFloat) { _.transform(_.float("f")).using(_.incrementFloat) }
    }

    // ---------------------------------------------------------------------------
    // transformUU
    {
      val c =
          cls("f".requiredUnion(
            string,
            single(cls("f".string, "g".int))))
        unionTest1(c)("""{"f":       "foo"}"""         ,            "FOO")
        unionTest2(c)("""{"f": {"f": "foo", "g": 1}}""", obj("f" -> "FOO", "g" -> 1)) { _.transformEntity("f").using(_.toUpperCase("f")) }
        unionTest2(c)("""{"f": {"f": "foo", "g": 1}}""", obj("f" -> "foo", "g" -> 2)) { _.transformEntity("f").using(_.increment  ("g")) }
      }

      // ---------------------------------------------------------------------------
      // transformUZ
      {
        val c =
            cls("f".requiredUnion(
              string,
              multiple(cls("f".string, "g".int))))

          unionTest1(c)("""{"f":        "foo"}""",                                                  "FOO")
          unionTest2(c)("""{"f": [{"f": "foo1", "g": 1}, {"f": "foo2", "g": 2}]}""", Seq(obj("f" -> "FOO1", "g" -> 1), obj("f" -> "FOO2", "g" -> 2))) { _.transformAllEntities("f").using(_.toUpperCase("f")) }
          unionTest2(c)("""{"f": [{"f": "foo1", "g": 1}, {"f": "foo2", "g": 2}]}""", Seq(obj("f" -> "foo1", "g" -> 2), obj("f" -> "foo2", "g" -> 3))) { _.transformAllEntities("f").using(_.increment  ("g")) }
      }

    // ---------------------------------------------------------------------------
    {
      val c =
            cls("f".requiredUnion(
              int,
              single(cls(name = "type1", Seq("_type".string, "f1".string, "g".int))),
              single(cls(name = "type2", Seq("_type".string, "f2".string, "h".int)))))

          unionTest2(c)("""{"f":                                       3 }""",                                                3 ) { identity }
          unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", "f1" -> "foo1", "g" -> 1)) { identity }
          unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", "f2" -> "foo2", "h" -> 3)) { identity }

        unionTest2(c)("""{"f":                                       3 }""",                                                4 ) { _.transform(_.int("f")).using(_ + 1) }
        unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", "f1" -> "FOO1", "g" -> 1)) { _.transformEntity("f").withTypeName("type1").using(_.toUpperCase("f1")) }
        unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", "f1" -> "foo1", "g" -> 2)) { _.transformEntity("f").withTypeName("type1").using(_.increment  ("g")) }
        unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", "f2" -> "FOO2", "h" -> 3)) { _.transformEntity("f").withTypeName("type2").using(_.toUpperCase("f2")) }
        unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", "f2" -> "foo2", "h" -> 4)) { _.transformEntity("f").withTypeName("type2").using(_.increment  ("h")) }
    }

    // ---------------------------------------------------------------------------
    {
      val c =
        cls("f".requiredUnion(
          string, // only difference
          single(cls(name = "type1", Seq("_type".string, "f1".string, "g".int))),
          single(cls(name = "type2", Seq("_type".string, "f2".string, "h".int)))))

      unionTest2(c)("""{"f":                          "foo"          }""",                                 "FOO"            ) { _.transform(_.string("f")).using(_.toUpperCase) }
      unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", "f1" -> "FOO1", "g" -> 1)) { _.transformEntity("f").withTypeName("type1").using(_.toUpperCase("f1")) }
      unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", "f1" -> "foo1", "g" -> 2)) { _.transformEntity("f").withTypeName("type1").using(_.increment  ("g")) }
      unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", "f2" -> "FOO2", "h" -> 3)) { _.transformEntity("f").withTypeName("type2").using(_.toUpperCase("f2")) }
      unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", "f2" -> "foo2", "h" -> 4)) { _.transformEntity("f").withTypeName("type2").using(_.increment  ("h")) }
    }

    // ---------------------------------------------------------------------------
    // using index
    {
      val c =
        cls("f".requiredUnion(
          int,
          single(cls("f1".string, "g".int)),
          single(cls("f2".string, "h".int))))

        unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj("f1" -> "foo1", "g" -> 1)) { identity }

        // ---------------------------------------------------------------------------
        unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj("f1" -> "FOO1", "g" -> 1)) { _.transformEntity("f").withIndex(0).using(_.toUpperCase("f1")) }

          unionTest2(c)("""{"f": 3                     }""", 3                            ) { _.transformEntity("f").withIndex(1).using(_.toUpperCase("f2")) }
          unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj("f1" -> "foo1", "g" -> 1)) { _.transformEntity("f").withIndex(1).using(_.toUpperCase("f2")) }
          unionTest2(c)("""{"f": {"f2": "foo2", "h": 2}}""", obj("f2" -> "FOO2", "h" -> 2)) { _.transformEntity("f").withIndex(1).using(_.toUpperCase("f2")) }

          // ---------------------------------------------------------------------------
          unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj("f1" -> "FOO1", "g" -> 1)) { _.transformEntity("f").withFieldHint("f1").using(_.toUpperCase("f1")) }

          unionTest2(c)("""{"f": 3                     }""", 3                            ) { _.transformEntity("f").withFieldHint("f2").using(_.toUpperCase("f2")) }
          unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj("f1" -> "foo1", "g" -> 1)) { _.transformEntity("f").withFieldHint("f2").using(_.toUpperCase("f2")) }
          unionTest2(c)("""{"f": {"f2": "foo2", "h": 2}}""", obj("f2" -> "FOO2", "h" -> 2)) { _.transformEntity("f").withFieldHint("f2").using(_.toUpperCase("f2")) }

        // ---------------------------------------------------------------------------
        unionTest2(c)("""{"f": {"f2": "foo2", "h": 2}}""", obj("f2" -> "FOO2", "h" -> 2)) { _.transformEntity("f").withFieldHint("f2").using(_.toUpperCase("f2")) }

        unionTest2(c)("""{"f": {"f2": "foo2", "h": 2}}""", obj("f2" -> "FOO2", "h" -> 2)) { _.transformEntity("f").withPredicate(c => c.contains("f2") && c.contains("h"), _.string_("f2").exists(_ == "foo2")).using(_.toUpperCase("f2")) }

  //    unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj("f1" -> "FOO1", "g" -> 1)) { _.transformEntity("f").using(_.toUpperCase("f1")) }
    }

    // ===========================================================================
    // using transformx
    unionTest2(c1)("""{"f": "foo"}""", "foo") { _.increment("f") }
    unionTest2(c1)("""{"f":  3   }""",  4   ) { _.increment("f") }

    // ===========================================================================
    // to non-union
    """{"f":  3   }""".read(_.schema(c1)).transform(_.int   ("f")).using(_.toString)                        ._assert(bobj("f" -> "3"))
    """{"f":  3   }""".read(_.schema(c1)).transform(_.int   ("f")).using(_.toString).assertNotUnionType("f")._assert(bobj("f" -> "3")) // recommended
    """{"f":  3   }""".read(_.schema(c1))                                           .assertIsUnionType ("f")._silentRun()

    """{"f": "foo"}""".read(_.schema(c1)).transform(_.string("f")).using(_.length)                          ._assert(bobj("f" ->  3 ))
    """{"f": "foo"}""".read(_.schema(c1)).transform(_.string("f")).using(_.length)  .assertNotUnionType("f")._assert(bobj("f" ->  3 )) // recommended
    """{"f": "foo"}""".read(_.schema(c1))                                           .assertIsUnionType ("f")._silentRun()
//todo ; error out
    // ===========================================================================
    val bb  = aobj(cls("f1".string_ , "f2".int_, "h".boolean))(obj("f1" -> "foo",                      "h" -> true))
    val bb2 = aobj(cls("f1".strings_, "f2".int_, "h".boolean))(obj("f1" -> Seq("foo1", "foo2"),            "h" -> true))
    val cc  = aobj(cls("f1".string_ , "f2".int_, "h".boolean))(obj(                             "f2" -> 3, "h" -> true))

    // ---------------------------------------------------------------------------
    val aa  = aobj(cls("f".requiredUnion(string,  int), "h".boolean))(obj("f" -> "foo",           "h" -> true))
    val aa2 = aobj(cls("f".requiredUnion(strings, int), "h".boolean))(obj("f" -> Seq("foo1", "foo2"), "h" -> true))

    val dd  = aobj(cls("f".requiredUnion(string,  int), "h".boolean))(obj("f" ->      3,              "h" -> true))

    // ---------------------------------------------------------------------------
    bb.fuseToUnion("f1", "f2").as("f")._assert(aa)
    cc.fuseToUnion("f1", "f2").as("f")._assert(dd)
//    aobj(cls("f1".string_, "f2".int_, "h".boolean))(obj("f1" -> "foo", "f2" -> 3, "h" -> true)).foo("f1", "f2", "f")._fail()
//    aobj(cls("f1".string_, "f2".int_, "h".boolean))(obj(                          "h" -> true)).foo("f1", "f2", "f")._fail()

    // ---------------------------------------------------------------------------
    aa.fissionFromUnion("f").as("f1", "f2")._assert(bb)
    dd.fissionFromUnion("f").as("f1", "f2")._assert(cc)

    aa2.fissionFromUnion("f").as("f1", "f2")._assert(bb2)
  }

  // ===========================================================================
  def unionTest1[T: WeakTypeTag](c: Cls)(s: String, t: T) = unionTest2(c)(s, t) { _.transform(_.string("f")).using(_.toUpperCase) }

    // ---------------------------------------------------------------------------
    def unionTest2[T: WeakTypeTag](c: Cls)(s: String, t: T)(f: HeadO => HeadO) = {
      s .read(_.schema(c)).pipe(f)
        ._assert2(
          origin   = s.read(_.schema(c))._forceResult,
          expected = AObj(c, obj("f" -> t))) }

    // ---------------------------------------------------------------------------
    def unionTest3[T: WeakTypeTag](input: AObj)(t: T)(f: HeadO => HeadO) = {
      input
        .identity.pipe(f)
        ._assert2(AObj(input.c, obj("f" -> t))) }
}

// ===========================================================================
