package galliatest.suites.single

import gallia._

// ===========================================================================
object CommonTransformationsTest extends gallia.testing.Suite {
  import TestDataO._

  // ---------------------------------------------------------------------------
  val TmpParseObject  = bobj('f -> "foo", 'g -> """{"h":true}""")  
  val TmpFormatObject = bobj('f -> "foo", 'g -> bobj('h -> true))

  val TmpParseArray  = bobj('f -> "foo", 'g -> """[{"h":true},{"h":false}]""")  
  val TmpFormatArray = bobj('f -> "foo", 'g -> Seq(bobj('h -> true), bobj('h -> false)))
  
  // ===========================================================================
  override def test() {    
    TmpParseObject.parseJsonObjectString('g).usingSchema(cls('h.boolean)).check(TmpFormatObject)
    TmpParseArray .parseJsonArrayString ('g).usingSchema(cls('h.boolean)).check(TmpFormatArray)

    TmpFormatObject.formatJsonObjectString('g).check(TmpParseObject)
    TmpFormatArray .formatJsonArrayString ('g).check(TmpParseArray)
  }

}

// ===========================================================================
