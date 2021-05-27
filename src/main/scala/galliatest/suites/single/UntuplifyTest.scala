package galliatest.suites.single

import gallia._

// ===========================================================================
object UntuplifyTest extends gallia.testing.Suite {

  override def test() {

    // ===========================================================================
    // untuplify1

    bobj('f -> Seq("foo", "3"))
      .untuplify1z('f ~> 'F).asNewKeys('str, 'int)
        .check(
      bobj('F -> bobj('str -> "foo", 'int ->  "3")))

    // ---------------------------------------------------------------------------
    bobj('f -> "foo|3")
      .untuplify1a('f ~> 'F).withSplitter(_.split("\\|")).asNewKeys('str, 'int)
        .check(
      bobj('F -> bobj('str -> "foo", 'int ->  "3")))

    bobj('f -> Seq("foo|3", "bar|4"))
      .untuplify1a('f ~> 'F).withSplitter(_.split("\\|")).asNewKeys('str, 'int)
        .check(
      bobj('F -> Seq(bobj('str -> "foo", 'int ->  "3"), bobj('str -> "bar", 'int ->  "4"))))

    bobj('f -> Seq("foo|3", "bar|4"))

      //.untuplify1c('f ~> 'F).withSeparator(entry = "|").as('str, 'int),
      .untuplify1a('f ~> 'F)
        .withSplitter("|").asNewKeys('str, 'int)
          .check(
      bobj('F -> Seq(
          bobj('str -> "foo", 'int ->  "3"),
          bobj('str -> "bar", 'int ->  "4")) ))

    // ---------------------------------------------------------------------------
    bobj('f -> "foo|3,bar|4")
      .untuplify1b('f ~> 'F)
        .withSplitters(",", "|")
          .asNewKeys('str, 'int)
            .check(
      bobj('F -> Seq(
          bobj('str -> "foo", 'int ->  "3"),
          bobj('str -> "bar", 'int ->  "4")) ))

  // ===========================================================================
  // untuplify2

      bobj('f -> Seq("str=foo", "int=3"))
        .untuplify2z('f ~> 'F)
          .withSplitter("=")
            .asNewKeys('str, 'int)
              .check(
      bobj('F -> bobj('str -> "foo", 'int -> "3")))

    // ---------------------------------------------------------------------------
      bobj('f -> "str=foo|int=3")
        .untuplify2a('f ~> 'F)
          .withSplitters("|", "=")
            .asNewKeys('str, 'int)
              .check(
      bobj('F -> bobj('str /* = */ -> "foo" /* ; */, 'int -> "3")))

    // ---------------------------------------------------------------------------
      bobj('f -> Seq("str=foo;int=3", "str=bar;int=4", "str=baz;int=5"))
        .untuplify2a('f ~> 'F)
          .withSplitters(";", "=")
            .asNewKeys('str, 'int)
              .check(
      bobj('F -> Seq(
          bobj('str /* = */ -> "foo" /* ; */, 'int -> "3"),
          bobj('str /* = */ -> "bar" /* ; */, 'int -> "4"),
          bobj('str /* = */ -> "baz" /* ; */, 'int -> "5"))))

    // ---------------------------------------------------------------------------
      bobj('f -> "str=foo;int=3,str=bar;int=4,str=baz;int=5")
        .untuplify2b('f ~> 'F)
          .withSplitters(",", ";", "=")
            .asNewKeys('str, 'int)
              .check(
      bobj('F -> /* , */ Seq(
          bobj('str /* = */ -> "foo" /* ; */, 'int -> "3"),
          bobj('str /* = */ -> "bar" /* ; */, 'int -> "4"),
          bobj('str /* = */ -> "baz" /* ; */, 'int -> "5"))))
  }

}

// ===========================================================================
