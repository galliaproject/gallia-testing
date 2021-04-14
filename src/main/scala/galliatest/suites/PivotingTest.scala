package galliatest.suites

import gallia._

// ===========================================================================
object PivotingTest extends gallia.testing.Suite {
  import TestDataS.{Default51, Default52}

  // ===========================================================================
  override def test() {
    testNoRowsPivot00(in = bobjs(
        bobj('f -> 1, 'g -> "a"),
        bobj('f -> 2, 'g -> "b"),
        bobj('f -> 1, 'g -> "a"),
        bobj('f -> 2, 'g -> "b")))

    // ---------------------------------------------------------------------------
    testNoRowsPivot0(out = AObj(
      cls('foo.int_, 'foo2.int_),
      obj('foo -> 1, 'foo2 -> 2)))

    // ---------------------------------------------------------------------------
    testNoRowsPivot(
      in = bobjs(
          bobj('f -> "f1", 'g -> "a", 'h -> 1),
          bobj('f -> "f1", 'g -> "a", 'h -> 2),
          bobj('f -> "f1", 'g -> "b", 'h -> 3),
          bobj('f -> "f2", 'g -> "a", 'h -> 4),
          bobj('f -> "f2", 'g -> "c", 'h -> 5) ),
      inDistinct = bobjs(
          bobj('f -> "f1", 'g -> "a", 'h -> 1),
          // [ removed middle 3 for uniqueness]
          bobj('f -> "f2", 'g -> "c", 'h -> 5) ),

      // ---------------------------------------------------------------------------
      out = aobjs(
          bobj('f -> "f1", 'a -> Seq(1, 2), 'b -> Seq(3)             ).toNonRequired('a, 'b),
          bobj('f -> "f2", 'a -> Seq(4   )            , 'c ->  Seq(5)).toNonRequired('a, 'c) ) )

    // ===========================================================================
    testPivotEmulation(
      in =
          bobjs(
            bobj('f1 -> "f11", 'f2 -> "f21", 'g1 -> "a", 'g2 -> "z", 'h1 -> 1, 'h2 ->  1),
            bobj('f1 -> "f11", 'f2 -> "f21", 'g1 -> "a", 'g2 -> "z", 'h1 -> 2, 'h2 ->  4),
            bobj('f1 -> "f11", 'f2 -> "f21", 'g1 -> "b", 'g2 -> "y", 'h1 -> 3, 'h2 ->  9),
            bobj('f1 -> "f12", 'f2 -> "f22", 'g1 -> "a", 'g2 -> "z", 'h1 -> 4, 'h2 -> 16),
            bobj('f1 -> "f12", 'f2 -> "f22", 'g1 -> "c", 'g2 -> "x", 'h1 -> 5, 'h2 -> 25) ),
      out =
          aobjs(
            bobj('f1 -> "f11", 'f2 -> "f21", 'a_z ->  9, 'b_y -> 27              ).toNonRequired('a_z, 'b_y),
            bobj('f1 -> "f12", 'f2 -> "f22", 'a_z -> 64            , 'c_x ->  125).toNonRequired('a_z,      'c_x) ) )

    // ===========================================================================
    testFoo(
        in = aobjs(cls('f.string, 'g.string, 'h.int_))(
          obj('f -> "f1", 'g -> "a", 'h -> 1),
          obj('f -> "f1", 'g -> "a"  /* 2 */),
          obj('f -> "f1", 'g -> "b", 'h -> 3),
          obj('f -> "f2", 'g -> "a", 'h -> 4),
          obj('f -> "f2", 'g -> "c", 'h -> 5) ),

        // ---------------------------------------------------------------------------
        out1 = aobjs(
            bobj('f -> "f1", 'a -> Seq(1), 'b -> Seq(3)             ).toNonRequired('a, 'b),
            bobj('f -> "f2", 'a -> Seq(4)            , 'c ->  Seq(5)).toNonRequired('a,    'c)),
        out2 = aobjs(
            bobj('f -> "f1", 'a -> -1, 'b -> 3            ).toNonRequired('a, 'b),
            bobj('f -> "f2", 'a -> 4            , 'c ->  5).toNonRequired('a,     'c)) )

  }


  // ===========================================================================
  private def testNoRowsPivot00(in: BObjs) {
    in.transformString('g).using(_ => "").group('f).by('g).pivot('f).column('g).asNewKeys('a, 'b).dataError(vldt.ErrorId.Runtime.EmptyKey)

    in.group('f).by('g).pivot('f).column('g).asNewKeys('a, 'b)             .check(bobj('a -> Seq(1, 1), 'b -> Seq(2, 2)).toNonRequired('a, 'b))
    in.group('f).by('g).unarrayEntries  ('g).asNewKeys('a, 'b).valueKey('f).check(bobj('a -> Seq(1, 1), 'b -> Seq(2, 2)).toNonRequired('a, 'b))
    in.group('f).by('g).unarrayBy0      ('g).asNewKeys('a, 'b) //FIXME: t210303103728 - meta and data disagree
  }

  // ===========================================================================
  private def testNoRowsPivot0(out: AObj) {
    Default52.unarrayEntries  ('f).asNewKeys("foo", "foo2").valueKey('g).dataError(vldt.ErrorId.Runtime.NotUnique)
    Default52.pivot('g).column('f).asNewKeys("foo", "foo2")             .dataError(vldt.ErrorId.Runtime.NotUnique)
    Default51.unarrayEntries  ('f).asNewKeys("foo", "foo2").valueKey('g).check(out)
    Default51.pivot('g).column('f).asNewKeys("foo", "foo2")             .check(out)

    //Default51.pivotValue('g).columns('f).asNewKeys("foo", "foo2")
    //Default51.pivotBy   ('f -> Seq("foo", "foo2"))
    //Default51.pivot('g).noAggregation.noRows.columns('f).asNewKeys("foo", "foo2")
  }

  // ===========================================================================
  private def testNoRowsPivot(in: BObjs, inDistinct: BObjs, out: AObjs) {
    in.pivot(_.int('h)).using(identity).rows('f).column('g).asNewKeys('a, 'b, 'c).check(out)
    in.pivot(_.int('h)).using(x => x  ).rows('f).column('g).asNewKeys('a, 'b, 'c).check(out)
    in.pivot(_.int('h)).noAggregation  .rows('f).column('g).asNewKeys('a, 'b, 'c).check(out)
  //in.pivot(      'h ).noAggregation  .rows('f).column('g).asNewKeys('a, 'b, 'c).check(out)
    in.pivot(      'h )                .rows('f).column('g).asNewKeys('a, 'b, 'c).check(out)

    // ---------------------------------------------------------------------------
    inDistinct.unarrayEntries  ('f).asNewKeys('f1, 'f2).valueKey('h).check(bobj('f1 -> 1, 'f2 -> 5).toNonRequired('f1, 'f2))
    inDistinct.pivot('h).column('f).asNewKeys('f1, 'f2)             .check(bobj('f1 -> 1, 'f2 -> 5).toNonRequired('f1, 'f2))

    inDistinct             .unarrayBy0('f    ).asNewKeys('f1  , 'f2                )                        .check(bobj('f1   -> bobj('g -> "a", 'h -> 1), 'f2   -> bobj('g -> "c", 'h -> 5)))
    inDistinct             .unarrayBy0('f, 'g).asNewKeys('f1_a,               'f2_c).withDefaultKeySeparator.check(bobj('f1_a -> bobj('h -> 1)           , 'f2_c -> bobj('h -> 5)))
    in.group('h).by('f, 'g).unarrayBy0('f, 'g).asNewKeys('f1_a, 'f1_b, 'f2_a, 'f2_c).withDefaultKeySeparator.check(bobj('f1_a -> bobj('h -> Seq(1, 2)), 'f1_b -> bobj('h -> Seq(3)), 'f2_a -> bobj('h -> Seq(4)), 'f2_c -> bobj('h -> Seq(5))))
  }

  // ===========================================================================
  private def testPivotEmulation(in: BObjs, out: AObjs) {

    // manual pivot 1 (meh)
    in
      .groupBy('f1, 'f2).as('_group1)
      .transform(   _.objz('_group1)).using {
        _ .groupBy('g1, 'g2).as('_group2)
          .transform(   _.objz('_group2)).using {
            _.squash(_.int('h1), _.int('h2)).using {
              _ .map { case (h1, h2) => h1 * h2 }
                .sum } }
          .fuse('g1, 'g2).as('g).using(_ + "_" + _)
          .pivot('_group2).column('g).asNewKeys('a_z, 'b_y, 'c_x) }
      .unnestAllFrom('_group1)
      .check(out)

    // ---------------------------------------------------------------------------
    // manual pivot 2 (better)
    in
      .fuse('g1, 'g2).as('g).using(_ + "_" + _)
      .fuse('h1, 'h2).as('h).using(_ * _)
      .groupBy('f1, 'f2)
      .transformGroupObjectsUsing {
        _ .sum('h)    .by('g)
          .pivot('h).column('g).asNewKeys('a_z, 'b_y, 'c_x) }
      .unnestAllFrom(_group)
      .check(out)

    // ---------------------------------------------------------------------------
    // actual pivot
    in
       .fuse('g1, 'g2).as('g).using(_ + "_" + _)
       .fuse('h1, 'h2).as('h).using(_ * _)
       .pivot(_.int('h)).usingSum.rows('f1, 'f2).column('g).asNewKeys('a_z, 'b_y, 'c_x)
     .check(out)
  }

  // ===========================================================================
  private def testFoo(in: AObjs, out1: AObjs, out2: AObjs) {
    in.pivot(       'h )                 .rows('f).column('g).asNewKeys('a, 'b, 'c).check(out1)
  //in.pivot(       'h ).noAggregation   .rows('f).column('g).asNewKeys('a, 'b, 'c).check(out1)
    in.pivot(_.int_('h)).noAggregation   .rows('f).column('g).asNewKeys('a, 'b, 'c).check(out1)
    in.pivot(_.int_('h)).using(_.flatten).rows('f).column('g).asNewKeys('a, 'b, 'c).check(out1)
  //if (false) in.pivot(       'h ) .using(_.map(_ + 1)).rows('f).column('g).asNewKeys('a, 'b, 'c).test

    // ---------------------------------------------------------------------------
  //in.                              pivot(_.int_('h)).using(_.map(_.getOrElse(-1)).product).rows('f).column('g).asNewKeys('a, 'b, 'c).check(out2)// not allowed (c200930125015)
    in.setDefaultFor('h).asValue(-1).pivot(_.int ('h)).using(_                     .product).rows('f).column('g).asNewKeys('a, 'b, 'c).check(out2)
  }

}

// ===========================================================================
