package galliatest.suites.single

import aptus.String_ // for .underscore
import gallia._

// ===========================================================================
object CotransformTest extends gallia.testing.Suite {
	import TestDataO.{Default01, Default15m, Default15p}

	// ---------------------------------------------------------------------------
  override def test() {
   Default01
      .cotransform(
          _.string('f))
        .asOverwrite
          .using { _.underscore }
        .check(
            bobj('f -> "foo_", 'g -> 1))

   Default01
      .cotransform(
          _.string('f),
          _.int   ('g))
        .as('f, 'g2)
          .using { (f, g) => (
              /* f  */ g.max(10),
              /* g2 */ f.underscore) }
        .check(
            bobj('f -> 10, 'g -> 1, 'g2 -> "foo_"))

    Default01
      .cotransform(
          _.string('f),
          _.int   ('g))
        .asOverwrite                   // difference
          .using { (f, g) => (
              /* f  */ g.max(10),
              /* g2 */ f.underscore) }
        .check(bobj('f -> 10,         'g -> "foo_"))

   Default01
      .cotransform(
          _.string('f),
          _.int   ('g))
        .as('f, 'g2, 'g3)
          .using { (f, g) => (
              /* f  */ g.max(10),
              /* g2 */ f.underscore("2"),
              /* g3 */ f.underscore("3")) }
        .check(
            bobj('f -> 10, 'g -> 1, 'g2 -> "foo_2", 'g3 -> "foo_3"))

    def cotransform1(x: AObj) =
      x .cotransform(
            _.string_('f),
            _.int    ('g))
          .as('f, 'g2, 'g3)
            .using { (f, g) => (
                /* f  */ g.max(10),
                /* g2 */ f.map(_.underscore("2")),
                /* g3 */ f.map(_.underscore("3")).getOrElse("-")) }

    cotransform1(Default15p)
      .check(
        aobj(
          cls('f.int  , 'g.int , 'h.boolean, 'g2.string_   , 'g3.string ))(
          obj('f -> 10, 'g -> 1, 'h -> true, 'g2 -> "foo_2", 'g3 -> "foo_3" )))

    cotransform1(Default15m)
      .check(
        aobj(
          cls('f.int  , 'g.int , 'h.boolean, 'g2.string_, 'g3.string ))(
          obj('f -> 10, 'g -> 1, 'h -> true,              'g3 -> "-" )))
  }

}

// ===========================================================================
