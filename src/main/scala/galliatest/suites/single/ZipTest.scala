package galliatest
package suites.single

import gallia._

// ===========================================================================
object ZipTest extends gallia.testing.Suite {

  override def test() {
    //bobj('f -> Seq("a", "b", "c"), 'g -> Seq("1", "2", "3").zip('f, 'g).underNewKey('p) // TODO: t210111113343 - add support for zip (non-strings) for consistency

    // ---------------------------------------------------------------------------
    def strictify(keys: Key*)(u: HeadU) = // TODO: t210110101000 - offer a "strict" zip (if expect no missing fields)
      u.transformObjects('p).using {
        _.convert(keys).toRequired }

    // ===========================================================================
    val ZipOutput = bobj('p -> Seq(
            bobj('f -> "a", 'g -> "1"),
            bobj('f -> "b", 'g -> "2"),
            bobj('f -> "c", 'g -> "3")))

    bobj('f -> "a,b,c", 'g -> "1,2,3")
        .zipStrings('f, 'g).splitBy(",").underNewKey('p)
        .pipe(strictify('f, 'g))
      .check(ZipOutput)

    /*
      // TODO: t210109142621 - unzip operation
      ZipOutput
          .unzipStrings('p).joinedBy(",") // TODO: what to do if contains other fields
        .check(bobj('f -> "a,b,c", 'g -> "1,2,3"))
    */

    aobj(cls('f.string, 'g.string_))(obj('f -> "a,b,c"))
        .zipStrings('f, 'g).splitBy(",").underNewKey('p)
        .setDefaultFor('p |> 'g).asValue("missing")
        .pipe(strictify('f)) // f only
      .check(bobj('p -> Seq(bobj('f -> "a", 'g -> "missing"), bobj('f -> "b", 'g -> "missing"), bobj('f -> "c", 'g -> "missing"))))

    // ---------------------------------------------------------------------------
    bobj(
          'f -> "f1,f2,f3",
          'g -> "g1,g2,g3",
          'h -> "h1")
        .zipStrings('f, 'g, 'h).splitBy(",").underNewKey('p)
        .pipe(strictify('f, 'g, 'h))
      .check(
        bobj('p -> Seq(
          bobj('f -> "f1", 'g -> "g1", 'h -> "h1"),
          bobj('f -> "f2", 'g -> "g2", 'h -> "h1"),
          bobj('f -> "f3", 'g -> "g3", 'h -> "h1")) ))

    bobj('f -> "f1,f2,f3", 'h -> "h1")
        .zipStrings('f, 'h).splitBy(",").underNewKey('p)
        .pipe(strictify('f, 'h))
    .check(
      bobj('p -> Seq(
          bobj('f -> "f1", 'h -> "h1"),
          bobj('f -> "f2", 'h -> "h1"),
          bobj('f -> "f3", 'h -> "h1"))) )

    bobj('f -> "f1,f2,f3", 'h -> "h1")
        .zipStrings('f ~> 'F, 'h).splitBy(",").underNewKey('p)
        .pipe(strictify('F, 'h))
      .check(bobj('p -> Seq(
          bobj('F -> "f1", 'h -> "h1"),
          bobj('F -> "f2", 'h -> "h1"),
          bobj('F -> "f3", 'h -> "h1")) ))

    bobj('f -> "f1", 'h -> "h1")
        .zipStrings('f ~> 'F, 'h).splitBy(",").underNewKey('p)
        .pipe(strictify('F, 'h))
      .check(bobj('p -> Seq(
        bobj('F -> "f1", 'h -> "h1")) ))

    // ===========================================================================
    /*private var */val tmp1     /*: Obj*/ = bobj('f -> Seq("f1", "f2", "f3"), 'g -> Seq("g1", "g2", "g3"))
    /*private var */val tmp3/*: Seq[Obj]*/ = Seq(bobj('f -> "f1", 'g -> "g1"), bobj('f -> "f2", 'g -> "g2"), bobj('f -> "f3", 'g -> "g3"))

    // ---------------------------------------------------------------------------
    bobj('f -> "f1,f2,f3", 'g -> "g1,g2,g3")
        .zipStrings('f, 'g).splitBy(",").underNewKey('p)
          .pipe(strictify('f, 'g))
      .check(bobj('p -> tmp3))

    bobj('f -> "f1,f2,f3", 'g -> "g1,g2,g3")
          .zipStrings('f, 'g).splitBy(",").underNewKey('p)
.pipe(strictify('f, 'g))
        .check(bobj('p -> tmp3))
  }

}

// ===========================================================================
