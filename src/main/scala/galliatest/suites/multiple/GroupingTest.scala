package galliatest.suites.multiple

import gallia._

// ===========================================================================
object GroupingTest extends gallia.testing.Suite {
import TestDataO._
import TestDataS._


//private val Default03b = bobj('p -> Default01b, 'z -> false)
  private val zzz1 = bobjs(Default03, bobj('p -> Default01b, 'z -> false), Default03)
  private val zzz2 = bobjs(bobj('z -> true, 'p -> 8), bobj('z -> false, 'p -> 4))

  // ---------------------------------------------------------------------------
  private val IJEC = bobjs(bobj('f -> "foo", 'g -> Seq(1, 1)), bobj('f -> "foo2", 'g -> Seq(2) ))
  private val IJED = bobjs(bobj('f -> "foo", _group -> Seq(bobj('g -> 1), bobj('g -> 1))), bobj('f -> "foo2", _group -> Seq(bobj('g -> 2)) ))
  private val IJEE = bobjs(bobj('f -> "foo", 'GROUP -> Seq(bobj('g -> 1), bobj('g -> 1))), bobj('f -> "foo2", 'GROUP -> Seq(bobj('g -> 2)) ))
 
  // ===========================================================================
  override def test() {
    zzz1.equivalents(res1)(
      _.group   ('p        ).by('z       ),
      _.groupOne(_.index(0)).by('z       ),
      _.groupOne(_.firstKey).by('z       ),
      _.groupOne(        _.firstKey).by(_.lastKey) ,
      _.grouping(_.field(_.firstKey).by(_.lastKey)) )

    // ---------------------------------------------------------------------------
    zzz1.equivalents(res2)(
      _.groupBy ('z),
      _.groupBy ('z).asDefault,
      _.groupBy ('z).as(_group),

      _.grouping(_.all.by('z)),
      _.grouping(_.all.by('z).as(_group)),
      _.grouping(_.all.by('z).asDefault ),
      _.grouping(_.all.by(_.lastKey)),

      _.groupBy(_.lastKey),
      _.groupBy(_.lastKey).asDefault )

    // ---------------------------------------------------------------------------
    Default52.group('g).by('f).check(
        bobjs(bobj('f -> "foo" , 'g -> Seq(1, 1)),
              bobj('f -> "foo2", 'g -> Seq(2) )) )

    yyy.group('g).by('f1).check(
      bobjs(
        bobj('f1 -> "foo1", 'g -> Seq(1, 1)),
        bobj('f1 -> "foo3", 'g -> Seq(3) )) )

    zzz1
        .group('p).by('z)
        .transformObjects('p).using {
          _.squash(_.string('f), _.int('g)).using {
            _ .map { case (f, g) => f.size + g }
              .sum } }
      .check(bobjs(
          bobj('z -> true , 'p -> 8),
          bobj('z -> false, 'p -> 6)))

    // ===========================================================================
    Default52.equivalents(IJEC)(
       _.group('g).by('f) )

    Default52.equivalents(IJED)(
      _.groupBy('f),
      _.groupBy('f).asDefault,
      _.grouping(_.all.by('f)           ),
      _.grouping(_.all.by('f).asDefault ) )

    Default52.equivalents(IJEE)(
      _.groupBy('f)          .as('GROUP),
      _.grouping(_.all.by('f).as('GROUP)) )

    // ===========================================================================
    yyy.equivalents(
        bobjs(
          bobj('f1 -> "foo1", 'g -> Seq(1, 1)),
          bobj('f1 -> "foo3", 'g -> Seq(3) ) ) )(
     _.group('g).by('f1) )

    // ---------------------------------------------------------------------------
    yyy.equivalents(
        bobjs(
            bobj('f1 -> "foo1", 'f2 -> "foo2", 'g -> Seq(1, 1)),
            bobj('f1 -> "foo3", 'f2 -> "foo4", 'g -> Seq(3) ) ))(
      _.group   ('g       ).byTheRest,
      _.groupOne(_.lastKey).byTheRest )

    // ---------------------------------------------------------------------------
    yyy.equivalents(
        bobjs(
            bobj('g -> 1, _group -> Seq(bobj('f1 -> "foo1", 'f2 -> "foo2"), bobj('f1 -> "foo1", 'f2 -> "foo2"))),
            bobj('g -> 3, _group -> Seq(bobj('f1 -> "foo3", 'f2 -> "foo4"))) ) )(
      _.group('f1, 'f2).by('g       ),
      _.group('f1, 'f2).by(_.lastKey),
      _.group('f1, 'f2).by(_.lastKey).asDefault,

      _.group(_.initKeys     ).by('g         ),
      _.group(_.indices(0, 1)).by('g         ),
      _.group(_.initKeys     ).by(_.lastKey  ),
      _.group(_.initKeys     ).by(_.index(-1)),

      _.grouping(_.fields(_.initKeys).byTheRest),

      _.add('h -> true).group   (         _.indices(0, 1)).by(_.indices(2, 3))           .remove('h),
      _.add('h -> true).group   (         _.indices(0, 1)).by(_.indices(2, 3)).asDefault .remove('h),
      _.add('h -> true).grouping(_.fields(_.indices(0, 1)).by(_.indices(2, 3)))          .remove('h),
      _.add('h -> true).grouping(_.fields(_.indices(0, 1)).by(_.indices(2, 3)).asDefault).remove('h),
    )    
 }

}

// ===========================================================================
