package galliatest.suites.multiple

import gallia._

// ===========================================================================
object AggregatingTest2 extends gallia.testing.Suite {
import TestDataO._
import TestDataS._


  // ===========================================================================
  override def test() {  

val aa = bobjs(
//FIXME: int not double
        bobj('f1 -> "foo1", 'g -> 2),
        bobj('f1 -> "foo3", 'g -> 3 ) )

    //yyy.agg(_.field(_.int('g)).by('f1).usingSuM2).check(aa)
//./agg0/agg/fluency/AggFunction.scala:      def usingSum2(implicit ev: O1 <:< Double): End = ???// = { using((x: Seq[Double]) => x.sum); new End {} }
//      def usingSum (implicit ev: O1 <:< Int   ): End = { using(_.map(_.asInstanceOf[Int]).sum); new End {} }
//      def usingSum2(implicit ev: O1 <:< Double): End = ???// = { using((x: Seq[Double]) => x.sum); new End {} }

     Default61.sum('g).by('f1).check(aa)
     //.agg(_.field(_.int('g)).by('f1).usingSuM2).check(aa)
//val yyy  = bobjs(Default06a, Default06b, Default06a)
//              val Default06a = bobj('f1 -> "foo1", 'f2 -> "foo2", 'g -> 1)
//              val Default06b = bobj('f1 -> "foo3", 'f2 -> "foo4", 'g -> 3)

//  val Default57  = aobjs(Default13p, Default13m, Default13p2) // f.string_
//    //  obj('f -> "foo", 'g -> 1))
//    //  obj(             'g -> 1))
//    //  obj('f -> "bar", 'g -> 2))
      //  obj('f -> "foo", 'g -> 1))


//      Default52.countDistinct('g).by('f).asDefault.check(bobjs(bobj('f -> "foo", _count -> 1), bobj('f -> "foo2", _count -> 1)))
      //Default52.countPresent ('g).by('f).asDefault.check(bobjs(bobj('f -> "foo", _count -> 1), bobj('f -> "foo2", _count -> 1)))
//      Default52.countMissing ('g).by('f).asDefault.check(bobjs(bobj('f -> "foo", _count -> 1), bobj('f -> "foo2", _count -> 1)))
//
//  //Default52.countBy('f)           .check(bobjs(bobj('f -> "foo", _count -> 2), bobj('f -> "foo2", _count -> 1)))
//    Default52.countBy('f).asDefault .check(bobjs(bobj('f -> "foo", _count -> 2), bobj('f -> "foo2", _count -> 1)))
//    Default52.countBy('f).as('COUNT).check(bobjs(bobj('f -> "foo", 'COUNT -> 2), bobj('f -> "foo2", 'COUNT -> 1)))
//

  /*
  @Test def sumXByY = testgN(o1212)(
      _.sum('f).by('g),
      _.agg(_.field('f).by('g).usingSum) )(
    z(o('g -> a, 'f -> 2),
      o('g -> b, 'f -> 4)) )

    if (false) {
      implicit def ecoij(x: HeadZ#_CountBy): HeadZ = ???
      //val xxx:HeadZ =
        Default52.countBy('f).remove('f)
    }
  */
  }

}

// ===========================================================================
