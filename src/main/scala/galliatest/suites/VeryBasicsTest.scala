package galliatest.suites

import gallia._
import gallia.vldt.ErrorId ._

// ===========================================================================
object VeryBasicsTest extends gallia.testing.Suite {
  import TestDataO.{Default00, Default01, Default03, Default06, Default09}

  // ===========================================================================
  override def test() {
    testRename1(
        in  = Default01)(
        out = bobj('F -> "foo", 'g -> 1))

    testRename2(in = Default03)

    // ---------------------------------------------------------------------------
    bobj('FooBar -> 1)       .renameToUpperCase('FooBar)  check bobj('FOOBAR -> 1)
    bobj(           'f -> 1 ).renameToUpperCase(      'f) check bobj(           'F -> 1 )
    bobj('p -> bobj('f -> 1)).renameToUpperCase('p |> 'f) check bobj('p -> bobj('F -> 1))

    testRetain(in = bobj('p -> bobj('f1 -> 1, 'f2 -> 2), 'f3 -> 3, 'z -> true))

    // ===========================================================================
    testRemove1(
      in  = Default01,
      out = Default00)
    testRemove2(in = Default06)

    // ---------------------------------------------------------------------------
    Default03.remove('p |> 'f) check bobj('p -> Default00, 'z -> true)
    //Default03.remove('p |> 'f ~> 'F) -- shouldn't compile anymore

    // ===========================================================================
    testAdd    (in = Default01)
    testReplace(in = Default01)

    // ===========================================================================
    { // mix
      implicit val in = Default09
        in.retain(_.allKeys)      noop

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

  // ===========================================================================
  private def testRename1(in: BObj)(out: BObj) {
    in.rename('f).to('F)  check out
    in.rename("f" ~> 'F)  check out
    in.rename('f).to("F") check out
    in.rename('f ~> "F" ) check out
  }

  // ---------------------------------------------------------------------------
  private def testRename2(in: BObj) {
    in.rename('p |> 'f ~> 'F ) check bobj('p -> bobj('F -> "foo", 'g -> 1), 'z -> true)
    in.rename('p |> 'f).to('F) check bobj('p -> bobj('F -> "foo", 'g -> 1), 'z -> true)
  //in.rename('p |> 'f).test() // shouldn't compile
  }

  // ===========================================================================
  private def testRetain(in: BObj) {
    in.retain(                      'f3,          'z) check bobj(                                'f3 -> 3, 'z -> true)
    in.retain('p,                                 'z) check bobj('p -> bobj('f1 -> 1, 'f2 -> 2),           'z -> true)
    in.retain('p |> 'f1,                          'z) check bobj('p -> bobj('f1 -> 1),                     'z -> true)
    in.retain(           'p |> 'f2,               'z) check bobj('p -> bobj(          'f2 -> 2),           'z -> true)
    in.retain('p |> 'f1, 'p |> 'f2,               'z) check bobj('p -> bobj('f1 -> 1, 'f2 -> 2),           'z -> true)
    in.retain(_.customKeys(_.tail))                   check bobj(                                'f3 -> 3, 'z -> true)
    in.retain('p ~> 'P)                               check bobj('P -> bobj('f1 -> 1, 'f2 -> 2))
    in.retain('p |> 'f1 ~> 'F1, 'p |> 'f2 ~> 'F2)     check bobj('p -> bobj('F1 -> 1, 'F2 -> 2))
    in.retain('p |> 'f1 ~> 'F1, 'p |> 'f2 ~> 'F2, 'z) check bobj('p -> bobj('F1 -> 1, 'F2 -> 2),           'z -> true)
  }

  // ===========================================================================
  private def testRemove1(in: BObj, out: BObj) {
  //in.remove('f ~> 'F) -- must not compile
    in.remove(           'f ) check out
    in.remove(_.explicit('f)) check out

    in.remove('f, 'g) metaError NoFieldsLeft
  }

  // ---------------------------------------------------------------------------
  private def testRemove2(in: BObj) {
    in.remove(_.allBut('f1     )) check bobj('f1 -> "foo")
    in.remove(_.allBut('f1, 'f2)) check bobj('f1 -> "foo", 'f2 -> "foo")

    in.remove('f1, 'f2) check Default00
  }

  // ===========================================================================
  private def testAdd(in: BObj) {
    in.add('f ->     "x"                          ) metaError FieldAlreadyExists
    in.add('f ->     bobj('z -> 1)                ) metaError FieldAlreadyExists

    in.add('p ->     bobj('z -> 1)                ) check bobj('f -> "foo", 'g -> 1, 'p ->     bobj('z -> 1)                )
    in.add('p -> Seq(bobj('z -> 1), bobj('z -> 2))) check bobj('f -> "foo", 'g -> 1, 'p -> Seq(bobj('z -> 1), bobj('z -> 2)))

    in.add('p ->     TestMeta.Foo("aa", "AA"))                                check bobj('f -> "foo", 'g -> 1, 'p ->     bobj('a -> "aa" , 'A -> "AA"))
    in.add('p -> Seq(TestMeta.Foo("aa1", "AA1"), TestMeta.Foo("aa2", "AA2"))) check bobj('f -> "foo", 'g -> 1, 'p -> Seq(bobj('a -> "aa1", 'A -> "AA1"), bobj('a -> "aa2", 'A -> "AA2")))

    in.add('f -> "FOO"               ) metaError FieldAlreadyExists
    in.add('z -> "z1", 'z -> "z2"    ) metaError DuplicateKeys
    in.add('z -> new java.io.File("")) metaError UnsupportedTlSubtype

    in.add('h -> true, "h2" -> 3) check bobj('f -> "foo", 'g -> 1, 'h -> true, 'h2 -> 3)

    in.addId("myid") check bobj('f -> "foo", 'g -> 1, _id -> "myid")
  }

  // ===========================================================================
  private def testReplace(in: BObj) {
    in.replace('f2 -> "FOO"               ) metaError NoSuchField
    in.replace('f  -> "z1", 'f -> "z2"    ) metaError DuplicateKeys

    in.replace('f -> "FOO") check bobj('f -> "FOO", 'g -> 1)
    in.replace('f ->     3) check bobj('f ->    3 , 'g -> 1)

    in.replace('f -> TestMeta.Foo("aa", "AA")) check bobj('f -> bobj('a -> "aa", 'A -> "AA"), 'g -> 1)
  }

}

// ===========================================================================
