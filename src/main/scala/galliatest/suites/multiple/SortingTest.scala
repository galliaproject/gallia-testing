package galliatest.suites.multiple

import gallia._

// ===========================================================================
object SortingTest extends gallia.testing.Suite {
import TestDataO._
import TestDataS._
import gallia.vldt._Error



private val Default02z = bobj('f -> Seq("foo3", "foo4"), 'g -> 3)    
private val Default60  = bobjs(Default02, Default02z)    


  override def test() {
    val o1 = bobj('f -> 1, 'g -> "a")
    val o2 = bobj('f -> 2, 'g -> "b")
    val o1212 = bobjs(o1, o2, o1, o2)
  
    // ===========================================================================
    Default52.distinct  .check(Default51)
  
    // ---------------------------------------------------------------------------
    // ensure uniqueness
    Default52       .ensureUniqueness          .dataError[_Error.Runtime.NotUnique]
    Default52       .ensureUniquenessBy('f    ).dataError[_Error.Runtime.NotUnique]
    Default52       .ensureUniquenessBy('f, 'g).dataError[_Error.Runtime.NotUnique]
  
    Default51.noop(_.ensureUniqueness)
    Default51.noop(_.ensureUniquenessBy('f))
    Default51.noop(_.ensureUniquenessBy('f, 'g))
  
    // ===========================================================================
    o1212.sortUsingUnsafe( _.int('f)).check(bobjs(o1, o1, o2, o2))
    o1212.sortUsingUnsafe(-_.int('f)).check(bobjs(o2, o2, o1, o1))
  
    o1212.sortBy    ('f)        .check(bobjs(o1, o1, o2, o2))
    o1212.sortBy    (_.firstKey).check(bobjs(o1, o1, o2, o2))
  
    o1212.sortUsing(_.int('f))              .using(-_)               .check(bobjs(o2, o2, o1, o1))
    o1212.sortUsing(_.int('f), _.string('g)).using((f, g) => (g, -f)).check(bobjs(o1, o1, o2, o2))
  
    o1212.sort.check(bobjs(o1, o1, o2, o2))
  
    o1212.sortBy('f        , 'g)       .check(bobjs(o1, o1, o2, o2))
    o1212.sortBy(_.firstKey, _.lastKey).check(bobjs(o1, o1, o2, o2))
  
    o1212.sortBy('f.desc)    .check(bobjs(o2, o2, o1, o1))
    o1212.sortBy('f.desc, 'g).check(bobjs(o2, o2, o1, o1))
  
    // ===========================================================================
    // descending
  
    Default52  .sortByDescendingFirstKey.check(bobjs(Default01b, Default01, Default01))
    Default52  .sortDescending          .check(bobjs(Default01b, Default01, Default01))
  
    Default52ef.sortByDescendingFirstKey.check(bobjs(Default01f, Default01e, Default01e, Default01g))
    Default52ef.sortDescending          .check(bobjs(Default01f, Default01g, Default01e, Default01e)) // only differing
  
    Default56  .sortByDescendingFirstKey.check(bobjs(Default06b, Default06a, Default06a))
    Default56  .sortDescending          .check(bobjs(Default06b, Default06a, Default06a))
  
    Default57  .sortByDescendingFirstKey.check(aobjs(Default13m, Default13p, Default13p, Default13p2))
    Default57  .sortDescending          .check(aobjs(Default13m, Default13p, Default13p, Default13p2))
  
    Default59  .sortByDescendingFirstKey.check(aobjs(Default14m, Default14p, Default14p, Default14p2))
    Default59  .sortDescending          .check(aobjs(Default14m, Default14p, Default14p, Default14p2))
  
    Default60  .sortByDescendingFirstKey.check(bobjs(Default02z, Default02))
    Default60  .sortDescending          .check(bobjs(Default02z, Default02))
  
    // ===========================================================================
    // missing last
  
    Default57.sortBy('f, descending = false, missingLast = true ).check(aobjs(Default13p2, Default13p, Default13p, Default13m))
    Default57.sortBy('f, descending = true , missingLast = true ).check(aobjs(Default13p, Default13p, Default13p2, Default13m))
    Default57.sortBy('f, descending = false, missingLast = false).check(aobjs(Default13m, Default13p2, Default13p, Default13p))
    Default57.sortBy('f, descending = true , missingLast = false).check(aobjs(Default13m, Default13p, Default13p, Default13p2))
  
    Default57.sort  (descending = false, missingLast = true ).check(aobjs(Default13p2, Default13p, Default13p, Default13m))
    Default57.sort  (descending = true , missingLast = true ).check(aobjs(Default13p, Default13p, Default13p2, Default13m))
    Default57.sort  (descending = false, missingLast = false).check(aobjs(Default13m, Default13p2, Default13p, Default13p))
    Default57.sort  (descending = true , missingLast = false).check(aobjs(Default13m, Default13p, Default13p, Default13p2))
  
    Default59.sortBy('f, descending = false, missingLast = true ).check(aobjs(Default14p2, Default14p, Default14p, Default14m))
    Default59.sortBy('f, descending = true , missingLast = true ).check(aobjs(Default14p, Default14p, Default14p2, Default14m))
    Default59.sortBy('f, descending = false, missingLast = false).check(aobjs(Default14m, Default14p2, Default14p, Default14p))
    Default59.sortBy('f, descending = true , missingLast = false).check(aobjs(Default14m, Default14p, Default14p, Default14p2))
  
    Default59.sort  (descending = false, missingLast = true ).check(aobjs(Default14p2, Default14p, Default14p, Default14m))
    Default59.sort  (descending = true , missingLast = true ).check(aobjs(Default14p, Default14p, Default14p2, Default14m))
    Default59.sort  (descending = false, missingLast = false).check(aobjs(Default14m, Default14p2, Default14p, Default14p))
    Default59.sort  (descending = true , missingLast = false).check(aobjs(Default14m, Default14p, Default14p, Default14p2))
  
    // ===========================================================================
    // multiple
  
    Default56.sortBy   (_.firstKey)   .check(bobjs(Default06a, Default06a, Default06b))
    Default56.sortByAll(_.initKeys)   .check(bobjs(Default06a, Default06a, Default06b))
    Default56.sortByAll(Seq('f1, 'f2)).check(bobjs(Default06a, Default06a, Default06b))
  
    // ===========================================================================
    val tmp210119: HeadZ = HeadZ.Dummy
  
      tmp210119.sortUsing(_.int('weight), _.string('target)).using((w, t) => (-w, t))
  
      tmp210119.sortUsingUnsafe { o => (-o.int('weight), o.string('target)) }
  
      tmp210119.sortUsingUnsafe { (x, y) =>
          val a = - x.int('weight)
          val b = - y.int('weight)
  
          val c = x.string('target)
          val d = y.string('target)
  
          implicitly[Ordering[(Int, String)]]
            .compare((a, c), (b, d)) }
  
      tmp210119.sortBy('weight.desc, 'target)
  
    // ---------------------------------------------------------------------------
    o1212.sortBy('f.useDescending).check(bobjs(o2, o2, o1, o1)) // .sortReverseNumericallyBy
  
    if (false) {
      //@deprecated def sort(conf: Conf.Start => Conf.End): Self = ???
      //    o1212.sort(_.by('f    )).check(bobjs(o2, o2, o1, o1))   //_.by('f, _.numerical)
      //    o1212.sort(_.by('f, 'g)).check(bobjs(o2, o2, o1, o1))   //_.by('f, _.numerical, 'g, _.alphabetical)
      //    o1212.sort(_.by('f, 'g)).check(bobjs(o2, o2, o1, o1)) } //_.by('f             , 'g, _.alphabetical)
    }
  }
}

// ===========================================================================
