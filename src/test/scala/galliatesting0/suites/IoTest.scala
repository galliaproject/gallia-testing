package galliatesting0
package suites

import gallia._

// ===========================================================================
object IoTest extends gallia.testing.Suite with gallia.testing.More {


  override def test() {   
    // TODO: port rest from old tests
    
    // ---------------------------------------------------------------------------
    Seq(("foo", 1), ("bar", 2))
      .toHead { case (f, g) => bobj("f" -> f, "g" -> g) }
        .check(bobjs(bobj('f -> "foo", 'g -> 1), bobj('f -> "bar", 'g -> 2)))

    // ---------------------------------------------------------------------------
    // foreach (TODO: actually check output)
    TestDataO.Default01.identity.foreach { _.printPrettyJson() }        
    TestDataS.Default51.identity.foreach { _.printPrettyJson() }
    
    // ---------------------------------------------------------------------------
    // print row/pretty-row (TODO: actually check output)
    TestDataO.Default01.identity.printRow()
    TestDataO.Default01.identity.printPrettyRow()
    
    TestDataS.Default51.identity.printPrettyJsons()
    
    TestDataO.Default01.identity.printRow()
  }

}

// ===========================================================================
