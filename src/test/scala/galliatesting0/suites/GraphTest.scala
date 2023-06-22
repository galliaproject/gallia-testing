package galliatesting0
package suites

import gallia._
import aptus.{Anything_, String_}

// ===========================================================================
object GraphTest extends gallia.testing.Suite with gallia.testing.More {

  override def test() {
    val in: HeadO = bobj("f" -> "foo", "g" -> 1)

      // regression test for core's 0aa5d8d8 bug fix
      in.rename("g" ~> "g1").convertToMultiple
        .innerJoin {
      in.rename("g" ~> "g2").convertToMultiple }
        .check(bobjs(bobj("f" -> "foo", "g1" -> 1, "g2" -> 1)))

    // ===========================================================================
    TestDataO.Default01
      .rename("f" ~> "F")
      .tap { forkee =>
        forkee.remove("F").check(bobj("g" -> 1))
        forkee.retain("F").check(bobj("F" -> "foo")) }

    // ---------------------------------------------------------------------------
    TestDataS.Default51
      .rename("f" ~> "F")
      .tap { forkee =>
        forkee.remove("F").check(bobjs(bobj("g" -> 1)    , bobj("g" -> 2)))
        forkee.retain("F").check(bobjs(bobj("F" -> "foo"), bobj("F" -> "foo2"))) }

    // ---------------------------------------------------------------------------
    val (leaf1, leaf2) =
      TestDataO.Default01
        .rename("f" ~> "F")
        .pipe { forkee =>
          forkee.remove("F").formatString ->
          forkee.retain("F").formatString }

    // TODO: part of test suite rather
    leaf1.compactJson.assert(_ == """{"g":1}""")
    leaf2.compactJson.assert(_ == """{"F":"foo"}""")

    // ===========================================================================
    // diamond tests

    TestDataO.Default01
        .rename("f" ~> "F")
        .pipe { forkee =>
          forkee.remove("F") ->
          forkee.retain("F") }
        .pipe { case (a, b) =>
          a.merge(b) }
        .check(bobj("g" -> 1, "F" -> "foo"))

    // ---------------------------------------------------------------------------
    def diamondZ(f: HeadZ => HeadZ) =
        TestDataS.Default51
          .rename("f" ~> "F")
          .pipe(f)
          .pipe { forkee =>
            forkee.remove("F") zipSameSize
            forkee.retain("F") }
          .check(bobjs(
            bobj("g" -> 1, "F" -> "foo"),
            bobj("g" -> 2, "F" -> "foo2")))

      diamondZ(_.toViewBased)
      diamondZ(_.toIteratorBased)

if (false)
        TestDataS.Default51
          .rename("f" ~> "F")
.toIteratorBased
          .pipe { forkee =>
            forkee.remove("F").display()
            forkee.retain("F").display() }

  }

}

// ===========================================================================
