package galliatesting0
package suites
package multiple

import gallia._

// ===========================================================================
object StatsTest extends gallia.testing.Suite with gallia.testing.More { import galliatesting0.suites.multiple.ReducingTest

  // ---------------------------------------------------------------------------
  override def test() {
    if (false) ReducingTest.Reducer2.statsOnAllKeys.formatCompactJson // FIXME: t220617101108

    ReducingTest.Reducer2.stats('g2).by('g1).check(
      bobjs(        
        bobj("g1" -> 1, "g2" -> bobj("_count_all" -> 2, "_distinct" -> 1, "_mean" -> 7.7, "_stdev" -> 0.0, "_min" -> 7.7, "_max" -> 7.7, "_median" -> 7.7) ),
        bobj("g1" -> 3, "g2" -> bobj("_count_all" -> 1, "_distinct" -> 1, "_mean" -> 4.4, "_stdev" -> 0.0, "_min" -> 4.4, "_max" -> 4.4, "_median" -> 4.4) ) ) )
      //_count_all -> 2, '_distinct -> 2, _mean -> 2.0, _stdev -> 1.0, _min -> 1, _max -> 3, _median -> 2.0)))

    ReducingTest.Reducer1
          .reduce('g1).wit(_.stats)
          .maxDecimals('g1 |> _stdev , 2)
        .check(bobj('g1 -> bobj(
          _count_all -> 2, '_distinct -> 2, _mean -> 2.0, _stdev -> 1.41 /* now use sample SD - vs population's, TODO: reconsider? */, 
          _min -> 1, _max -> 3, _median -> 2.0)))

    // ===========================================================================
    ReducingTest.Reducer2
      .statsOnAllKeys
      .forEachKey('g1, 'g2, 'g3).thn { (u1, k1) =>  //TODO: generalize for stats
        u1.forEachKey(_mean, _stdev).thn { (u2, k2) =>
          u2.maxDecimals(k1 |> k2 , 2) } }
      .checkDataOnly( // TODO: check schema too (after t201207170908 - cc mapping)
        obj(
          /* now use sample SD - vs population's, TODO: reconsider? */
          'g1 -> obj(_count_all -> 3, '_distinct -> 2,                 _mean ->  1.67, _stdev -> 1.15, _min -> 1  , _max -> 3  , _median ->  1.0),
          'g2 -> obj(_count_all -> 3, '_distinct -> 2,                 _mean ->  6.6 , _stdev -> 1.91, _min -> 4.4, _max -> 7.7, _median ->  7.7),
          'g3 -> obj(_count_all -> 3, '_distinct -> 3, '_present -> 2, _mean -> 15.0 , _stdev -> 7.07, _min -> 10 , _max -> 20 , _median -> 15.0),
          'g4 -> obj(_count_all -> 3, '_distinct -> 2,                 '_values -> Seq(obj('_value -> "foo1", _count_all -> 2), obj('_value -> "foo3", _count_all -> 1)))) )

    // ===========================================================================
    if (false) // t220404150923
    	//@deprecated("see 210118083814 for new version")
      TestDataS.Default52ef.stats('g).by('f)
          //.find(_.string('f)).using(_ == "foo")
          //                .maxDecimals(_stats |> _mean , 2) // - FIXME
          //                .maxDecimals(_stats |> _stdev, 2)
        .checkDataOnly(gallia.Objs.splat(
            obj('f -> "foo",  _stats -> obj(_mean -> 1.4666666666666668, _stdev -> 0.5185449728701349)),
            obj('f -> "foo2", _stats -> obj(_mean -> 2.2 , _stdev -> 0.0 ))))
    // custom num agg
  	//reuse g rather? or offer to?
  }

}

// ===========================================================================
