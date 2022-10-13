package galliatest.suites.single

import gallia._
import gallia.vldt.ErrorId ._
import gallia.vldt._Error

// ===========================================================================
object VeryBasicsTest extends gallia.testing.Suite {
  import TestDataO.{Default00, Default01, Default03, Default03b, Default06, Default06a, Default09}

  // ===========================================================================
  override def test() {
    
    // ===========================================================================
    // reordering keys
    
    Default01.reorderKeys           (_.reverse).check(                       bobj('g -> 1, 'f -> "foo") )
    Default01.reverseKeyOrder                  .check(                       bobj('g -> 1, 'f -> "foo") )
    Default03.reorderKeysRecursively(_.reverse).check(bobj('z -> true, 'p -> bobj('g -> 1, 'f -> "foo")))
  
    Default01.reorderKeys           (_.tail).metaError[_Error.InvalidKeyReordering]

    Default06a.reorderAsFirstKeys(_.lastKey)          .check(bobj('g -> 1, 'f1 -> "foo1" , 'f2 -> "foo2"))
    Default06a.reorderAsFirstKeys(_.explicit('g))     .check(bobj('g -> 1, 'f1 -> "foo1" , 'f2 -> "foo2"))    
    Default06a.reorderAsFirstKeys(           'g)      .check(bobj('g -> 1, 'f1 -> "foo1" , 'f2 -> "foo2"))
    Default06a.reorderAsFirstKeys(           'g, 'f2 ).check(bobj('g -> 1,                 'f2 -> "foo2" , 'f1 -> "foo1"))
    Default06a.reorderAsFirstKeys(_.explicit('g, 'f2)).check(bobj('g -> 1,                 'f2 -> "foo2" , 'f1 -> "foo1"))

    Default06a.reorderAsLastKeys(_.firstKey)          .check(bobj( 'f2 -> "foo2", 'g -> 1, 'f1 -> "foo1"))
    Default06a.reorderAsLastKeys(_.explicit('f1))     .check(bobj( 'f2 -> "foo2", 'g -> 1, 'f1 -> "foo1"))
    Default06a.reorderAsLastKeys(           'f1)      .check(bobj( 'f2 -> "foo2", 'g -> 1, 'f1 -> "foo1"))
    Default06a.reorderAsLastKeys(           'f2, 'f1 ).check(bobj(                'g -> 1, 'f1 -> "foo1", 'f2 -> "foo2"))
    Default06a.reorderAsLastKeys(_.explicit('f2, 'f1)).check(bobj(                'g -> 1, 'f1 -> "foo1", 'f2 -> "foo2"))
    
    // ===========================================================================
    // rename

    Default01.rename('f).to('F) .check(bobj('F -> "foo", 'g -> 1))
    Default01.rename("f" ~> 'F) .check(bobj('F -> "foo", 'g -> 1))
    Default01.rename('f).to("F").check(bobj('F -> "foo", 'g -> 1))
    Default01.rename('f ~> "F" ).check(bobj('F -> "foo", 'g -> 1))
  
    // ---------------------------------------------------------------------------
    Default03.rename('p |> 'f ~> 'F ).check(bobj('p -> bobj('F -> "foo", 'g -> 1), 'z -> true))
    Default03.rename('p |> 'f).to('F).check(bobj('p -> bobj('F -> "foo", 'g -> 1), 'z -> true))
  //Default03.rename('p |> 'f).test() // shouldn't compile

    // ---------------------------------------------------------------------------
    bobj('FooBar -> 1)       .renameToUpperCase('FooBar) .check(bobj('FOOBAR -> 1))
    bobj(           'f -> 1 ).renameToUpperCase(      'f).check(bobj(           'F -> 1 ))
    bobj('p -> bobj('f -> 1)).renameToUpperCase('p |> 'f).check(bobj('p -> bobj('F -> 1)))

    // ===========================================================================
    // retain
    
    val retainIn = Default03b

      retainIn.retain(                      'f3,          'z).check(bobj(                                'f3 -> 3, 'z -> true))
      retainIn.retain('p,                                 'z).check(bobj('p -> bobj('f1 -> 1, 'f2 -> 2),           'z -> true))
      retainIn.retain('p |> 'f1,                          'z).check(bobj('p -> bobj('f1 -> 1),                     'z -> true))
      retainIn.retain(           'p |> 'f2,               'z).check(bobj('p -> bobj(          'f2 -> 2),           'z -> true))
      retainIn.retain('p |> 'f1, 'p |> 'f2,               'z).check(bobj('p -> bobj('f1 -> 1, 'f2 -> 2),           'z -> true))
      retainIn.retain(_.customKeys(_.tail))                  .check(bobj(                                'f3 -> 3, 'z -> true))
      retainIn.retain('p ~> 'P)                              .check(bobj('P -> bobj('f1 -> 1, 'f2 -> 2)))
      retainIn.retain('p |> 'f1 ~> 'F1, 'p |> 'f2 ~> 'F2)    .check(bobj('p -> bobj('F1 -> 1, 'F2 -> 2)))
      retainIn.retain('p |> 'f1 ~> 'F1, 'p |> 'f2 ~> 'F2, 'z).check(bobj('p -> bobj('F1 -> 1, 'F2 -> 2),           'z -> true))

retainIn.retain(_.indices(0, 1))// TODO: add test    
retainIn.retain(_.index  (0))   // TODO: add test    

    // ===========================================================================
  //Default01.remove('f ~> 'F) -- must not compile
    Default01.remove(           'f ) check Default00
    Default01.remove(_.explicit('f)) check Default00
    Default01.remove(_.index(0))     check Default00
Default01.remove(_.indices(0, 1))// TODO: add test

    Default01.remove('f, 'g) metaError NoFieldsLeft    

    // ---------------------------------------------------------------------------
    Default06.remove(_.allBut('f1     )) check bobj('f1 -> "foo")
    Default06.remove(_.allBut('f1, 'f2)) check bobj('f1 -> "foo", 'f2 -> "foo")

    Default06.remove('f1, 'f2) check Default00
    
    // ---------------------------------------------------------------------------
    Default03.remove('p |> 'f) check bobj('p -> Default00, 'z -> true)
    //Default03.remove('p |> 'f ~> 'F) -- shouldn't compile anymore

    // ===========================================================================
    // add
    
    Default01.add('h -> true) check bobj('f -> "foo", 'g -> 1, 'h -> true)

//TODO: allow? - Default01.add('p >| 'f ->     "x"                          ) - see t210111113206    

    Default01.add('f ->     "x"                          ) metaError FieldAlreadyExists
    Default01.add('f ->     bobj('z -> 1)                ) metaError FieldAlreadyExists


    
    Default01.add('p ->     bobj('z -> 1)                ) check bobj('f -> "foo", 'g -> 1, 'p ->     bobj('z -> 1)                )
    Default01.add('p -> Seq(bobj('z -> 1), bobj('z -> 2))) check bobj('f -> "foo", 'g -> 1, 'p -> Seq(bobj('z -> 1), bobj('z -> 2)))

    Default01.add('p ->     TestMeta.Foo("aa", "AA"))                                check bobj('f -> "foo", 'g -> 1, 'p ->     bobj('a -> "aa" , 'A -> "AA"))
    Default01.add('p -> Seq(TestMeta.Foo("aa1", "AA1"), TestMeta.Foo("aa2", "AA2"))) check bobj('f -> "foo", 'g -> 1, 'p -> Seq(bobj('a -> "aa1", 'A -> "AA1"), bobj('a -> "aa2", 'A -> "AA2")))

    Default01.add('f -> "FOO"               ) metaError FieldAlreadyExists
    Default01.add('z -> "z1", 'z -> "z2"    ) metaError DuplicateKeys
    Default01.add('z -> new java.io.File("")) metaError UnsupportedTlSubtype

    Default01.add('h -> true, "h2" -> 3) check bobj('f -> "foo", 'g -> 1, 'h -> true, 'h2 -> 3)

    Default01.addId("myid") check bobj('f -> "foo", 'g -> 1, _id -> "myid")
    
    // ===========================================================================
    // replace
    Default01.replace('f2 -> "FOO"               ) metaError NoSuchField
    Default01.replace('f  -> "z1", 'f -> "z2"    ) metaError DuplicateKeys

    Default01.replace('f -> "FOO") check bobj('f -> "FOO", 'g -> 1)
    Default01.replace('f ->     3) check bobj('f ->    3 , 'g -> 1)

    Default01.replace('f -> TestMeta.Foo("aa", "AA")) check bobj('f -> bobj('a -> "aa", 'A -> "AA"), 'g -> 1)

    // ===========================================================================
    { // mix
      implicit val in = Default09
        //in.noop(_.retain(_.allKeys))

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

}

// ===========================================================================
