package galliatesting0
package suites
package single

import gallia._

// ===========================================================================
object DeserializeTest extends gallia.testing.Suite with gallia.testing.More { // formerly UntuplifyTest

  import enumeratum.{Enum, EnumEntry}
  sealed trait DeserializeEnum extends EnumEntry
    object DeserializeEnum extends Enum[DeserializeEnum] {  
      val values = findValues
  
      // ---------------------------------------------------------------------------
      case object str extends DeserializeEnum
      case object int extends DeserializeEnum
    }  
  
  // ===========================================================================
  override def test() {

    // ---------------------------------------------------------------------------
    // deserialize1

    bobj('f -> Seq("foo", "3"))
      .deserialize1z('f ~> 'F).asNewKeys[DeserializeEnum]
        .check(
      bobj('F -> bobj('str -> "foo", 'int ->  "3")))
    
    bobj('f -> Seq("foo", "3"))
      .deserialize1z('f ~> 'F).asNewKeys('str, 'int)
        .check(
      bobj('F -> bobj('str -> "foo", 'int ->  "3")))

    // ---------------------------------------------------------------------------
    bobj('f -> "foo|3")
      .deserialize1a('f ~> 'F).withSplitter(_.split("\\|")).asNewKeys('str, 'int)
        .check(
      bobj('F -> bobj('str -> "foo", 'int ->  "3")))

    bobj('f -> Seq("foo|3", "bar|4"))
      .deserialize1a('f ~> 'F).withSplitter(_.split("\\|")).asNewKeys('str, 'int)
        .check(
      bobj('F -> Seq(bobj('str -> "foo", 'int ->  "3"), bobj('str -> "bar", 'int ->  "4"))))

    bobj('f -> Seq("foo|3", "bar|4"))

      //.deserialize1c('f ~> 'F).withSeparator(entry = "|").as('str, 'int),
      .deserialize1a('f ~> 'F)
        .withSplitter("|").asNewKeys('str, 'int)
          .check(
      bobj('F -> Seq(
          bobj('str -> "foo", 'int ->  "3"),
          bobj('str -> "bar", 'int ->  "4")) ))

    // ---------------------------------------------------------------------------
    bobj('f -> "foo|3,bar|4")
      .deserialize1b('f ~> 'F)
        .withSplitters(",", "|")
          .asNewKeys('str, 'int)
            .check(
      bobj('F -> Seq(
          bobj('str -> "foo", 'int ->  "3"),
          bobj('str -> "bar", 'int ->  "4")) ))

  // ===========================================================================
  // deserialize2

      bobj('f -> Seq("str=foo", "int=3"))
        .deserialize2z('f ~> 'F)
          .withSplitter("=")
            .asNewKeys('str, 'int)
              .check(
      bobj('F -> bobj('str -> "foo", 'int -> "3")))

    // ---------------------------------------------------------------------------
      bobj('f -> "str=foo|int=3")
        .deserialize2a('f ~> 'F)
          .withSplitters("|", "=")
            .asNewKeys('str, 'int)
              .check(
      bobj('F -> bobj('str /* = */ -> "foo" /* ; */, 'int -> "3")))

    // ---------------------------------------------------------------------------
      bobj('f -> Seq("str=foo;int=3", "str=bar;int=4", "str=baz;int=5"))
        .deserialize2a('f ~> 'F)
          .withSplitters(";", "=")
            .asNewKeys('str, 'int)
              .check(
      bobj('F -> Seq(
          bobj('str /* = */ -> "foo" /* ; */, 'int -> "3"),
          bobj('str /* = */ -> "bar" /* ; */, 'int -> "4"),
          bobj('str /* = */ -> "baz" /* ; */, 'int -> "5"))))

    // ---------------------------------------------------------------------------
      bobj('f -> "str=foo;int=3,str=bar;int=4,str=baz;int=5")
        .deserialize2b('f ~> 'F)
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
