package galliatesting0
package suites

import gallia._

// ===========================================================================
object TargetSelectionTest extends gallia.testing.Suite with gallia.testing.More {	
  import TestDataO._
      
  override def test() {
    // TODO:Default00.transform(_.string(_.soleKey)).using(_.reverse).check(Default01c)
    Default01.transform(_.string(_.firstKey)).using(_.reverse).check(Default01c)

    Default01.transform(_.typed [    String ]("f")).using(      _.reverse ).check(Default01c)
    Default02.transform(_.typedx[    String] ("f")).using(      _.reverse ).check(Default02y)
    Default02.transform(_.typed [Seq[String]]("f")).using(_.map(_.reverse)).check(Default02y)  
    
    bobj("value" -> byteBuffer("foo")).transform(_.binary("value")).using(_.mapBytes(_.reverse)).check(bobj("value" -> byteBuffer("oof")))
  }

}

// ===========================================================================
