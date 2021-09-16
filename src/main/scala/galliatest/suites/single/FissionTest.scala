package galliatest.suites.single

import aptus._
import gallia._

// ===========================================================================
object FissionTest extends gallia.testing.Suite {
  import TestDataO.Default01

  // ---------------------------------------------------------------------------
  override def test() {
    Default01
        .fission('f)
          .as('f1, 'f2)
            .using { s =>
                (s + "1", s + "2") }
          .check(bobj('g -> 1, 'f1 -> "foo1", 'f2 -> "foo2"))
          
      Default01
        .fission('f)
          .as('f1, 'f2)
            .using { s =>
              (s.sizeString.decrement, s.sizeString.increment) }        
          .check(bobj('g -> 1, 'f1 -> 2, 'f2 -> 4))

      Default01
        .fission('f)
          .as('f1, 'f2)
            .using { s =>
              (s.sizeString - 1, s.sizeString + 1) }        
          .check(bobj('g -> 1, 'f1 -> 2, 'f2 -> 4))

      Default01
        .fission('f)
          .as('f1, 'f2)
            .using { s =>
              (s.toString.size - 1, s.toString.size + 1) }        
          .check(bobj('g -> 1, 'f1 -> 2, 'f2 -> 4))
          
    // ---------------------------------------------------------------------------
    Default01
        .fission('f)
          .as('f1, 'f2, 'f3)
            .using { s =>
                (s + "1", s + "2", s + "3") }
          .check(bobj('g -> 1, 'f1 -> "foo1", 'f2 -> "foo2", 'f3 -> "foo3"))

    // ===========================================================================
    Default01
      .fission(
          _.string('f))
        .as('f1, 'f2)
          .using { s => (
              s.head.str,
              s.tail) }
        .check(bobj('g -> 1, 'f1 -> "f", 'f2 -> "oo"))
          
  }

}

// ===========================================================================
