package galliatest.suites.multiple

import gallia._

// ===========================================================================
object AggregatingTest extends gallia.testing.Suite {
import TestDataO._
import TestDataS._


  // ===========================================================================
  override def test() {  
    assert(Default52.forceSize == 3) // f is z

      // ---------------------------------------------------------------------------
      Default02b.toSize('f).check(bobj('f -> 3  , 'g -> 1)) // f is a seq

      // ---------------------------------------------------------------------------
      Default02b.toSum ('f).check(bobj('f -> 6  , 'g -> 1)) // f is ints
      Default02c.toSum ('f).check(bobj('f -> 6.6, 'g -> 1)) // f is doubles

if (false)//FIXME
Default02b.toSum ('f ~> 'F).check(bobj('F -> 6  , 'g -> 1)) // f is ints

      // ---------------------------------------------------------------------------
      Default02b.toMean('f)                   .check(bobj('f -> 2.0               , 'g -> 1))
      Default02c.toMean('f).maxDecimals('f, 2).check(bobj('f -> 2.2, 'g -> 1))

      // ---------------------------------------------------------------------------
      Default02b.toStdev('f).maxDecimals('f, 2).check(bobj('f -> 0.82, 'g -> 1))
      Default02c.toStdev('f).maxDecimals('f, 2).check(bobj('f -> 0.9 , 'g -> 1)) // TODO...

    // ===========================================================================
    Default52.countBy              ('f)           .check(bobjs(bobj('f -> "foo", _count -> 2), bobj('f -> "foo2", _count -> 1)))
    Default52.countBy              ('f).asDefault .check(bobjs(bobj('f -> "foo", _count -> 2), bobj('f -> "foo2", _count -> 1)))
    Default52.countBy              ('f).as('COUNT).check(bobjs(bobj('f -> "foo", 'COUNT -> 2), bobj('f -> "foo2", 'COUNT -> 1)))
    Default52.count   ('g      ).by('f)           .check(bobjs(bobj('f -> "foo", 'g     -> 2), bobj('f -> "foo2", 'g     -> 1)))
    Default52.count   ('g ~> 'G).by('f)           .check(bobjs(bobj('f -> "foo", 'G     -> 2), bobj('f -> "foo2", 'G     -> 1)))

    // ---------------------------------------------------------------------------
    Default56.countWith(_.count).by('g).check(bobjs(bobj('g -> 1, _count -> 2), bobj('g -> 3, _count -> 1)))
    Default56.countBy              ('g).check(bobjs(bobj('g -> 1, _count -> 2), bobj('g -> 3, _count -> 1)))

      Default57.count    ('f).by('g)                         .check(bobjs(bobj('g -> 1, 'f     -> 3), bobj('g -> 2, 'f     -> 1)))
      Default57.countBy                      ('g).as("COUNT").check(bobjs(bobj('g -> 1, 'COUNT -> 3), bobj('g -> 2, 'COUNT -> 1)))
      Default57.countBy                      ('g)            .check(bobjs(bobj('g -> 1, _count -> 3), bobj('g -> 2, _count -> 1)))
      Default57.countWith        (_.count).by('g)            .check(bobjs(bobj('g -> 1, _count -> 3), bobj('g -> 2, _count -> 1)))
      Default57.aggregate('f).wit(_.count).by('g)            .check(bobjs(bobj('g -> 1, 'f     -> 3), bobj('g -> 2, 'f     -> 1)))

//if (false)Default57.countPresentBy('g1, 'g2)

      Default57.countDistinctBy                       ('g).check(bobjs(bobj('g -> 1, _count -> 2), bobj('g -> 2, _count -> 1)))
      Default57.countWith        (_.count_distinct).by('g).check(bobjs(bobj('g -> 1, _count -> 2), bobj('g -> 2, _count -> 1)))
      Default57.aggregate('f).wit(_.count_distinct).by('g).check(bobjs(bobj('g -> 1, 'f     -> 2), bobj('g -> 2, 'f     -> 1)))

      Default57.countPresentBy                       ('g).check(bobjs(bobj('g -> 1, _count -> 2), bobj('g -> 2, _count -> 1)))
      Default57.countWith        (_.count_present).by('g).check(bobjs(bobj('g -> 1, _count -> 2), bobj('g -> 2, _count -> 1)))
      Default57.aggregate('f).wit(_.count_present).by('g).check(bobjs(bobj('g -> 1, 'f     -> 2), bobj('g -> 2, 'f     -> 1)))

      Default57.countWith        (_.count_distinct_present).by('g).check(bobjs(bobj('g -> 1, _count -> 1), bobj('g -> 2, _count -> 1)))
      Default57.aggregate('f).wit(_.count_distinct_present).by('g).check(bobjs(bobj('g -> 1, 'f     -> 1), bobj('g -> 2, 'f     -> 1)))

      Default57.countWith        (_.count_missing).by('g).check(bobjs(bobj('g -> 1, _count -> 1), bobj('g -> 2, _count -> 0)))
      Default57.aggregate('f).wit(_.count_missing).by('g).check(bobjs(bobj('g -> 1, 'f     -> 1), bobj('g -> 2, 'f     -> 0)))

    // ---------------------------------------------------------------------------
    // sum/count/... all by
//FIXME: Default57.sumAllBy     ('f).test__//(bobjs(bobj('g -> 1, _count -> 2), bobj('g -> 2, _count -> 1)))

    // ---------------------------------------------------------------------------
    Default52  .count('g)       .by('f)                   .check(bobjs(bobj('f -> "foo", 'g -> 2   ), bobj('f -> "foo2", 'g -> 1  )))
    Default52  .count(_.lastKey).by('f)                   .check(bobjs(bobj('f -> "foo", 'g -> 2   ), bobj('f -> "foo2", 'g -> 1  )))
    Default52  .sum  ('g)       .by('f)                   .check(bobjs(bobj('f -> "foo", 'g -> 2   ), bobj('f -> "foo2", 'g -> 2  )))
    Default52  .sum  ('g ~> 'G) .by('f)                   .check(bobjs(bobj('f -> "foo", 'G -> 2   ), bobj('f -> "foo2", 'G -> 2  )))
    Default52ef.mean ('g)       .by('f).maxDecimals('g, 2).check(bobjs(bobj('f -> "foo", 'g -> 1.47), bobj('f -> "foo2", 'g -> 2.2)))
    Default52ef.aggregate('g).wit(_.stdev)      .by('f).maxDecimals('g, 2).check(bobjs(bobj('f -> "foo", 'g -> 0.52), bobj('f -> "foo2", 'g -> 0.0)))

    // ===========================================================================
    // each

    val AA =
        bobjs(
            bobj('f -> "foo1", 'g1 -> 1, 'g2 -> 6),
            bobj('f -> "foo2", 'g1 -> 2, 'g2 -> 5),
            bobj('f -> "foo1", 'g1 -> 3, 'g2 -> 4))

       AA.sum('g1).by('f).check(
         bobjs(
             bobj('f -> "foo1", 'g1 -> 4),
             bobj('f -> "foo2", 'g1 -> 2) ))

     // ---------------------------------------------------------------------------
     val aaa1 =
         bobjs(
             bobj('f -> "foo1", '_sums -> bobj('g1 -> 4, 'g2 -> 10)),
             bobj('f -> "foo2", '_sums -> bobj('g1 -> 2, 'g2 ->  5 )) )

       AA.sumEach('g1, 'g2)  .by('f).check(aaa1)
       AA.sumEach(_.tailKeys).by('f).check(aaa1)

       AA.sumEach('g1, 'g2).by('f).as('COUNTS).check(
         bobjs(
             bobj('f -> "foo1", 'COUNTS -> bobj('g1 -> 4, 'g2 -> 10)),
             bobj('f -> "foo2", 'COUNTS -> bobj('g1 -> 2, 'g2 ->  5 )) ))

     val aaa2 =
         bobjs(
             bobj('f -> "foo1", '_counts -> bobj('g1 -> 2, 'g2 -> 2)),
             bobj('f -> "foo2", '_counts -> bobj('g1 -> 1, 'g2 -> 1)) )

      AA.sumEach  ('g1, 'g2)  .by('f).check(aaa1)
      AA.countEach('g1, 'g2)  .by('f).check(aaa2)
      AA.aggregateEach('g1, 'g2).wit(_.sum).by('f).check(aaa1)
  }

}

// ===========================================================================
