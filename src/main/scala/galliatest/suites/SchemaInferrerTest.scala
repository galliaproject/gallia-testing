package galliatest.suites

import aptus.Anything_ // for .thn/.assert
import gallia._

// ===========================================================================
object SchemaInferrerTest extends gallia.testing.Suite {	
  import gallia.inferring.SchemaInferrer._

  // ===========================================================================
  val QuickTestData =
    """
      {"first_name": "John", "last_name": "Johnson", "DOB": "1986-02-04", "height": "180", "manager": "true",  "confidential":"has crush on Kate"}
      {"first_name": "Kate", "last_name": "Smith"  , "DOB": "1987-02-04", "height": "165", "manager": "true",  "confidential":"has crush on John"}
      {"first_name": "Jack", "last_name": "Jackson", "DOB": "1986-02-04", "height": "160", "manager": "false", "confidential":"dfd", "yo": 1}
    """
  
  // ===========================================================================
  override def test() {
    TestDataO.Default01.rename('f ~> 'F).check(bobj('F -> "foo", 'g -> 1)) // so we have one test at least...

    // ---------------------------------------------------------------------------
    QuickTestData
      .streamObjs().toListAndTrash
      .thn(klass)
      .assert(_ == cls(
        'first_name        .string,
        'last_name         .string,
        'DOB               .string,
        'height            .string,
        'manager           .string,
        'confidential      .string,
        'yo                .int_   ))

    // ---------------------------------------------------------------------------
    klass(
        obj('foo -> "bar", 'baz -> 3, 'qux -> Seq(true, false), 'quux -> Seq(
          obj('a -> 0, 'c -> "c"),
          obj('a -> 1, 'b -> true)) ))
      .assert(_ ==
        cls(
          'foo               .string,
          'baz               .int,
          'qux               .booleans,
          'quux              .clss(
              'a                 .int     ,
              'c                 .string_ ,
              'b                 .boolean_)))

     // ---------------------------------------------------------------------------
     """
          {"f": "foo1", "g": 1}
          {"f": "foo2", "g": 2}
          {"f": "foo3", "g": 3}
        """
        .streamObjs().toListAndTrash
        .thn(klass)
        .assert(_ == cls('f.string, 'g.int))

    // ---------------------------------------------------------------------------
    """
         {"f": "foo1", "g": 1}
         {"f": "foo2", "g": 2}
         {"f": "foo3", "g": 3.3}
         {"f": "foo4", "g": 4}
        """
        .streamObjs().toListAndTrash
        .thn(klass)
        .assert(_ == cls('f.string, 'g.double))

    // ---------------------------------------------------------------------------
    """
         {"f": "foo1", "g": 1.1}
         {"f": "foo2", "g": 2}
         {"f": "foo3", "g": 3}
         {"f": "foo4", "g": 4}
       """
        .streamObjs().toListAndTrash
        .thn(klass)
        .assert(_ == cls('f.string, 'g.double))
  }

}

// ===========================================================================
