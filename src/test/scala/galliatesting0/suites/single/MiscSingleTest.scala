package galliatesting0
package suites
package single

import gallia._

// ===========================================================================
object MiscSingleTest extends gallia.testing.Suite with gallia.testing.More {

  import TestDataO.{Default01, Default09}

  // ===========================================================================
  override def test() {

    { // mix
      implicit val in = Default09
        //in.noop(_.retain(_.allKeys))

        in.remove(_.explicit('f)) check bobj('p -> Default01, 'z -> true)
        in.retain(_.explicit('f)) check bobj(                                      'f -> "foo")
      //in.remove(_.explicit('f ~> 'F)) check ? -- no longer valid
    }

    // ---------------------------------------------------------------------------
    {
      val in = Default01

      in.rename('f ~> 'F)       check bobj('F -> "foo", 'g -> 1)
      in.retain('f ~> 'F)       check bobj('F -> "foo")
      in.remove('f)             check bobj('g -> 1)
      in.remove(_.explicit('f)) check bobj('g -> 1)
      in.remove(_.index(0))     check bobj('g -> 1)
    }

    // ===========================================================================
//bobj('p -> bobj('f1 -> 1, 'f2 -> 2), 'f3 -> 3, 'z -> true).retain( 'f3, 'z) check bobj('f3 -> 4, 'z -> true)  // intentional error
  }
}
