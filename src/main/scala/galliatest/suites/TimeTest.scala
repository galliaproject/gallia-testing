package galliatest
package suites

import aptus._
import java.time.ZoneId

// ===========================================================================
object TimeTest extends gallia.testing.Suite {
  val instant = java.time.Instant.now()

  // ---------------------------------------------------------------------------
  private def test[T: WTT](value: T) = gallia.bobj("value" -> value).forceAObj

  // ---------------------------------------------------------------------------
  override def test() { import sandbox._; TestDataO.Default01.noop(_.identity)
		test(instant)
        .transform(_.instant("value")).using {
        _.atZone(ZoneId.systemDefault).getYear }
      ._assert2(gallia.bobj("value" -> 2022))

    "time: OK".p
  }
}

// ===========================================================================
