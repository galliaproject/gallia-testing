package galliatest.suites.single

// ===========================================================================
object HeadVTest extends gallia.testing.Suite {
  import TestDataO._
  import gallia.{bobj, _vle, headV}

  // ---------------------------------------------------------------------------
  override def test() {
    headV(3)              .check(             3 )
    headV(3).dressUp(_vle).check(bobj(_vle -> 3))

    headV("foo").concatenate("bar")   .check("foobar")
    headV("foo").concatenate(headV(1)).check("foo1")

    import aptus.Anything_
      headV(Seq (1, 2, 3)).format.assert(_ == "1\n2\n3\n")
      headV(List(1, 2, 3)).format.assert(_ == "1\n2\n3\n")
      headV(     1, 2, 3 ).format.assert(_ == "1\n2\n3\n")
      headV(           3 ).format.assert(_ ==       "3")

      //      headV(Seq (1, 2, 3)).println()
      //      headV(           3 ).println()
      //
      //      headV(Seq (1, 2, 3)).write("/tmp/here1.gz")
      //      headV(           3 ).write("/tmp/here2.gz")

    // ---------------------------------------------------------------------------
    //FIXME: 220920155343 - see question
    headV(1.1).combine(headV(1)).using(_ + _).check(2.1)
    headV(1.1).plus(1.1)                     .check(2.2)
    //if (false) headV(1.1).plus   (headV(1)).check(2.1) // FIXME: t220916154103
    //if (false) headV(1.1).plus(1).check(2.1)

    headV(List(1, 2, 3)).display()

    headV(1.1).plus(headV(1.1)).display()
    headV(1.1).plus      (1.1) .display()

    // ---------------------------------------------------------------------------
    (headV("foo").dressUp("f") merge headV(1).dressUp("g")).check(Default01)
  }
}

// ===========================================================================
