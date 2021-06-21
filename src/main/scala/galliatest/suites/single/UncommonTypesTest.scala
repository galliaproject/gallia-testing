package galliatest.suites.single

import aptus._
import gallia._

// ===========================================================================
object UncommonTypesTest extends gallia.testing.Suite {
	import TestDataO._

	// ---------------------------------------------------------------------------
  override def test() {
    bobj('value -> new java.lang.Long(3)).noop(_.identity)

    bobj('value ->       BigInt    (3)).noop(_.identity)
    bobj('value ->       BigDecimal(3)).noop(_.identity)
    bobj('value -> scala.BigDecimal(3)).noop(_.identity)

    bobj('value -> "2021-01-08"         .date)    .noop(_.identity) // {"value":{"year":2021,"month":1,"day":8}} - does it work th eother way around?
    bobj('value -> "2021-01-08T01:02:03".datetime).noop(_.identity) // {"value":{"date":{"year":2021,"month":1,"day":8},"time":{"hour":1,"minute":2,"second":3,"nano":0}}}

    //bobj('f -> 3, 'value -> TestEnumeratum.enum_value1).identity.test
    //    bobj('value -> TestScalaEnum .enum_value1).identity.test
  }

}

// ===========================================================================
