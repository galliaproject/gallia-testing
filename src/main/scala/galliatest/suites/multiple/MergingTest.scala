package galliatesting0
package suites
package multiple

import gallia._

// ===========================================================================
object MergingTest extends gallia.testing.Suite with gallia.testing.More {
  private val left  = bobjs(bobj('f -> "f1", 'g -> "a"), bobj('f -> "f2" , 'g -> "b"), bobj('f -> "f2", 'g -> "a"))

  // ---------------------------------------------------------------------------
  private val right1 = bobjs(bobj('f -> "f1", 'G -> "a"), bobj('f -> "f2b", 'G -> "b"))
  private val right2 = right1.union(bobj('f -> "f1") /* add conflict */)
  private val right3 = right1.union(bobj('g -> "a"   /* add conflict */))
  private val right4 = bobjs(bobj('f -> "f1", 'G -> "a", 'z -> true), bobj('f -> "f2b", 'G -> "b", 'z -> false))

  // ===========================================================================
  override def test() {
    testUnion(mem = true)
    testUnion(mem = false)

    testZipSameSize(mem = true)
    testZipSameSize(mem = false)

    // ---------------------------------------------------------------------------
    testBring()
    testBringAll()
    testLeftCoGroup()
    testLeftJoin()
    testFullCoGroup(
        expected = aobjs(cls('f.string, _left.clss_('g.string), _right.clss_('G.string)))(
          obj('f -> "f1" , _left -> Seq(obj('g -> "a"))                 , _right -> Seq(obj('G -> "a"))),
          obj('f -> "f2" , _left -> Seq(obj('g -> "b"), obj('g -> "a"))),
          obj('f -> "f2b",                                                _right -> Seq(obj('G -> "b")))) )

    // ---------------------------------------------------------------------------
    bobj('f -> "foo", 'g -> 1).convertToMultiple.innerJoin(bobj('f -> "foo").convertToMultiple).check(bobjs(bobj('f -> "foo", 'g -> 1)))

    // ===========================================================================
    def tmp0(mem: Boolean) = {
        val left  = TestDataS.Default52.rename("f" ~> "f1").rename("g" ~> "g1").forceAObjs._toBased(mem)
        val right = TestDataS.Default52.rename("f" ~> "f2").rename("g" ~> "g2").forceAObjs._toBased(mem)

        left -> right
      }

      // ---------------------------------------------------------------------------
      def tmp1(mem: Boolean) = { val (left, right) = tmp0(mem); left.join   (right, "f1" <~> "f2") }
      def tmp2(mem: Boolean) = { val (left, right) = tmp0(mem); left.coGroup(right, "f1" <~> "f2") }

      // ---------------------------------------------------------------------------
      val expected1 =
        bobjs(
          bobj("f1" -> "foo", "g1" -> 1, "g2" -> 1),
          bobj("f1" -> "foo", "g1" -> 1, "g2" -> 1),
          bobj("f1" -> "foo", "g1" -> 1, "g2" -> 1),
          bobj("f1" -> "foo", "g1" -> 1, "g2" -> 1),
          bobj("f1" -> "foo2", "g1" -> 2, "g2" -> 2))

      // ---------------------------------------------------------------------------
      val expected2 =
        bobjs(
          bobj("f1" -> "foo", _left -> Seq(bobj("g1" -> 1), bobj("g1" -> 1)), _right -> Seq(bobj("g2" -> 1), bobj("g2" -> 1))),
          bobj("f1" -> "foo2" , _left -> Seq(bobj("g1" -> 2))                 , _right -> Seq(bobj("g2" -> 2))) )

      // ---------------------------------------------------------------------------
      tmp1(mem = false).check(expected1)
      tmp1(mem = true) .check(expected1)

      tmp2(mem = false).check(expected2)
      tmp2(mem = true) .check(expected2)
  }

  // ===========================================================================
  private def testUnion(mem: Boolean) {
    left._toBased(mem)
        .union(
          bobj('f -> "f3", 'g -> "c"))
      .check(
          bobj('f -> "f1", 'g -> "a"),
          bobj('f -> "f2", 'g -> "b"),
          bobj('f -> "f2", 'g -> "a"),
          bobj('f -> "f3", 'g -> "c"))

    // ---------------------------------------------------------------------------
    (left._toBased(mem), right1._toBased(mem))
        .union
      .check(cls('f.string, 'g.string_, 'G.string_))(
        obj('f -> "f1", 'g -> "a"), obj('f -> "f2" , 'g -> "b"), obj('f -> "f2", 'g -> "a"),
        obj('f -> "f1", 'G -> "a"), obj('f -> "f2b", 'G -> "b") )
  }

  // ===========================================================================
  private def testZipSameSize(mem: Boolean) {
    val x = bobjs(bobj("f" -> "foo1"), bobj("f" -> "foo2"))._toBased(mem)
    val y = bobjs(bobj("g" ->     1 ), bobj("g" ->     2 ))._toBased(mem)

    x.zipSameSize(y)
      .check(
        bobj("f" -> "foo1", 'g -> 1),
        bobj("f" -> "foo2", 'g -> 2))
  }

  // ===========================================================================
  private def testBring() {
    left
        .bring(right4, 'G)
      .check(cls('f.string, 'g.string, 'G.string_))(
          obj('f -> "f1", 'g -> "a", 'G -> "a"),
          obj('f -> "f2", 'g -> "b"),
          obj('f -> "f2", 'g -> "a") )
  }

  // ===========================================================================
  private def testBringAll() {
    left
        .bringAll(right4)
      .check(cls('f.string, 'g.string, 'G.string_, 'z.boolean_))(
          obj('f -> "f1", 'g -> "a", 'G -> "a", 'z -> true),
          obj('f -> "f2", 'g -> "b"),
          obj('f -> "f2", 'g -> "a") )

     // ---------------------------------------------------------------------------
    if (false) {
                                        //TODO: ensure proper failure
                                          left
                                              .bringAll(right2, via = 'f)
                                              //.merging(right1)(_.bring.all.via('f))

                                        //TODO: add a noop
    }
  }

  // ===========================================================================
  private def testLeftCoGroup() {
    left
      .leftCoGroup(right1, on = 'f).flattenByBoth
      .check(cls('f.string, _left.cls('g.string), _right.cls_('G.string)))(
          obj('f -> "f1" , _left -> obj('g -> "a") , _right -> obj('G -> "a")),
          obj('f -> "f2" , _left -> obj('g -> "b")),
          obj('f -> "f2" , _left -> obj('g -> "a")) )
  }

  // ===========================================================================
  private def testLeftJoin() {
    left
        .leftJoin(right1)
      .check(cls('f.string, 'g.string, 'G.string_))(
          obj('f -> "f1" , 'g -> "a" , 'G -> "a"),
          obj('f -> "f2" , 'g -> "b"),
          obj('f -> "f2" , 'g -> "a"))

    // ---------------------------------------------------------------------------
    if (false) {
      //FIXME?
      //failg(left)(_
          left
            .leftJoin(right3, on = 'f)
            //.fail("po sur")
      //fail(left)(_.leftJoin(right :+ o('g -> a /* add conflict */), on = 'f) )
    }
  }

  // ===========================================================================
  private def testFullCoGroup(expected: AObjs) {
    left
        .fullCoGroup(right1, on = 'f)
      .check(expected)

    // ---------------------------------------------------------------------------
    left
        .fullCoGroup(right1, on = 'f, asLeft = 'LEFT, asRight = 'RITE)
      .check(
        expected
           .rename(
              _left  ~> 'LEFT,
              _right ~> 'RITE)
           ._forceResult)
  }

}

// ===========================================================================
