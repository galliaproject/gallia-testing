package galliatest.suites.multiple

import gallia._

// ===========================================================================
object StatsTest extends gallia.testing.Suite {
	import galliatest.suites.multiple.ReducingTest

	// ---------------------------------------------------------------------------
  override def test() {  

    //@deprecated("see 210118083814 for new version")
    TestDataS.Default52ef.stats('g).by('f)
        //.find(_.string('f)).using(_ == "foo")
        //        .maxDecimals(_stats |> _mean , 2) - FIXME
        //        .maxDecimals(_stats |> _stdev, 2)
      .checkDataOnly(gallia.Objs.splat(
          obj('f -> "foo",  _stats -> obj(_mean -> 1.4666666666666668, _stdev -> 0.5185449728701349)),
          obj('f -> "foo2", _stats -> obj(_mean -> 2.2 , _stdev -> 0.0 ))))
    // custom num agg
//should reuse g? or offer to?

  // ---------------------------------------------------------------------------
  ReducingTest.Reducer1.reduce('g1).wit(_.stats).check(bobj('g1 -> bobj(
      _count -> 2, '_distinct -> 2, _mean -> 2.0, _stdev -> 1.0, _min -> 1, _max -> 3, _median -> 2.0)))

    // ===========================================================================
    ReducingTest.Reducer2
      .stats
      .forEachKey('g1, 'g2, 'g3).zen { (u1, k1) =>  //TODO: generalize for stats
        u1.forEachKey(_mean, _stdev).zen { (u2, k2) =>
          u2.maxDecimals(k1 |> k2 , 2) } }
      .checkDataOnly( // TODO: check schema too (after t201207170908 - cc mapping)
        obj(
          'g1 -> obj(_count -> 3, '_distinct -> 2,                 _mean ->  1.67, _stdev -> 0.94, _min -> 1  , _max -> 3  , _median ->  1.0),
          'g2 -> obj(_count -> 3, '_distinct -> 2,                 _mean ->  6.6 , _stdev -> 1.56, _min -> 4.4, _max -> 7.7, _median ->  7.7),
          'g3 -> obj(_count -> 3, '_distinct -> 3, '_present -> 2, _mean -> 15.0 , _stdev -> 5.0 , _min -> 10 , _max -> 20 , _median -> 15.0),
          'g4 -> obj(_count -> 3, '_distinct -> 2,                 '_values -> Seq(obj('_value -> "foo1", _count -> 2), obj('_value -> "foo3", _count -> 1)))) )
  }

}

// ===========================================================================
