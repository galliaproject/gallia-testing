package galliatesting0
package suites
package single

import gallia._

// ===========================================================================
object CommonTransformationsTest extends gallia.testing.Suite with gallia.testing.More {
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

    // ===========================================================================
    bobj('f -> 4.0).transformDouble ('f)    .using(math.sqrt)   .check(bobj('f -> 2.0))
  //bobj('f -> 4.0).transformDoublex('f)    .using(math.sqrt)   .check(bobj('f -> 2.0))
    bobj('f -> 4.0).sqrt  ('f)                                  .check(bobj('f -> 2.0))
    bobj('f -> 4.0).square('f)                                  .check(bobj('f -> 16.0))

    bobj('f -> 4.0).log('f).check(bobj('f -> 1.3862943611198906))

    bobj('f -> 0.12345).maxDecimals('f, 2).check(bobj('f -> 0.12))
    bobj('f -> 2.19999).maxDecimals('f, 2).check(bobj('f -> 2.2 ))
    
    // ---------------------------------------------------------------------------
    aobj(cls('f1.double  , 'g.int))(obj('f1 ->     8.0      , 'g -> 1)).square('f1).check(aobj(cls('f1.double  , 'g.int))(obj('f1 ->     64.0       , 'g -> 1)))
    aobj(cls('f1.double_ , 'g.int))(obj('f1 ->     8.0      , 'g -> 1)).square('f1).check(aobj(cls('f1.double_ , 'g.int))(obj('f1 ->     64.0       , 'g -> 1)))
    aobj(cls('f1.double_ , 'g.int))(obj(                      'g -> 1)).square('f1).check(aobj(cls('f1.double_ , 'g.int))(obj(                        'g -> 1)))
    aobj(cls('f1.doubles , 'g.int))(obj('f1 -> Seq(7.0, 9.0), 'g -> 1)).square('f1).check(aobj(cls('f1.doubles , 'g.int))(obj('f1 -> Seq(49.0, 81.0), 'g -> 1)))
    aobj(cls('f1.doubles_, 'g.int))(obj('f1 -> Seq(7.0, 9.0), 'g -> 1)).square('f1).check(aobj(cls('f1.doubles_, 'g.int))(obj('f1 -> Seq(49.0, 81.0), 'g -> 1)))
    aobj(cls('f1.doubles_, 'g.int))(obj(                      'g -> 1)).square('f1).check(aobj(cls('f1.doubles_, 'g.int))(obj(                        'g -> 1)))

    // ---------------------------------------------------------------------------    
  }

}

// ===========================================================================
