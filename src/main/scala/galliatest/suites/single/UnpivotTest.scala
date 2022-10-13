package galliatest.suites.single

import gallia._

// ===========================================================================
object UnpivotTest extends gallia.testing.Suite {

  override def test() {
    val in  = bobj('f -> "foo", 'g -> 1,                           "a" ->       "A",              "b" ->       "B",              "c" ->       "C")
    val out = bobj('f -> "foo", 'g -> 1, _group -> Seq(bobj(_id -> "a", _vle -> "A"), bobj(_id -> "b", _vle -> "B"), bobj(_id -> "c", _vle -> "C")))
    in.unpivot("a", "b", "c").check(out)

    // ---------------------------------------------------------------------------
    if (false) {
      TestDataS.Default52
        .pivone("f").asNewKeys("foo", "foo2")
        .check(
          bobj(
            "foo"  -> bobjs(bobj('g -> 1), bobj('g -> 1)),
            "foo2" -> bobjs(bobj('g -> 2))))

    }

    // ---------------------------------------------------------------------------
    TestDataO.Default04.unpivotOneItem("p", "f").withValue("foo").check(
        bobj('p -> Seq(bobj('f -> "foo2", 'g -> 2)), 'z -> true, "foo" -> bobj('g -> 1)))

      TestDataO.Default04p.unpivotOneItem("p", "f").withValue("foo").check(aobj(
          cls('p.clss_     ('f.string,    'g.int),   'z.boolean, 'foo.cls_   ('g.int)))(
          obj('p -> Seq(obj('f -> "foo2", 'g -> 2)), 'z -> true, "foo" -> obj('g -> 1))))

      TestDataO.Default04m.unpivotOneItem("p", "f").withValue("foo").check(aobj(
          cls('p.clss_     ('f.string,    'g.int),   'z.boolean, 'foo.cls_   ('g.int)))(
          obj(                                       'z -> true)))
  }

}

// ===========================================================================
