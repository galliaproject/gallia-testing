package galliatest.suites.multiple

import gallia._

// ===========================================================================
object MergingTest extends gallia.testing.Suite {
  private val left  = bobjs(bobj('f -> "f1", 'g -> "a"), bobj('f -> "f2" , 'g -> "b"), bobj('f -> "f2", 'g -> "a"))

  // ---------------------------------------------------------------------------
  private val right1 = bobjs(bobj('f -> "f1", 'G -> "a"), bobj('f -> "f2b", 'G -> "b"))
  private val right2 = right1.union(bobj('f -> "f1") /* add conflict */)
  private val right3 = right1.union(bobj('g -> "a"   /* add conflict */))
  private val right4 = bobjs(bobj('f -> "f1", 'G -> "a", 'z -> true), bobj('f -> "f2b", 'G -> "b", 'z -> false))

  // ===========================================================================
  override def test() {
    testUnion()
    testBring()
    testBringAll()
    testLeftCoGroup()
    testLeftJoin()
    testFullCoGroup(
        expected = aobjs(cls('f.string, _left.clss_('g.string), _right.clss_('G.string)))(
          obj('f -> "f1" , _left -> Seq(obj('g -> "a"))                 , _right -> Seq(obj('G -> "a"))),
          obj('f -> "f2" , _left -> Seq(obj('g -> "b"), obj('g -> "a"))),
          obj('f -> "f2b",                                                _right -> Seq(obj('G -> "b")))) )
  }

  // ===========================================================================
  private def testUnion() {
    left
        .union(
          bobj('f -> "f3", 'g -> "c"))
      .check(
          bobj('f -> "f1", 'g -> "a"),
          bobj('f -> "f2", 'g -> "b"),
          bobj('f -> "f2", 'g -> "a"),
          bobj('f -> "f3", 'g -> "c"))

    // ---------------------------------------------------------------------------
    (left, right1)
        .union
      .check(cls('f.string, 'g.string_, 'G.string_))(
        obj('f -> "f1", 'g -> "a"), obj('f -> "f2" , 'g -> "b"), obj('f -> "f2", 'g -> "a"),
        obj('f -> "f1", 'G -> "a"), obj('f -> "f2b", 'G -> "b") )
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
           .forceAObjs)
  }

}

// ===========================================================================
