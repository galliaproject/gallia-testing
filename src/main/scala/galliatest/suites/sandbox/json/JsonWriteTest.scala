package galliatest
package suites
package sandbox
package json

import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import gallia._
import aptus._

// ===========================================================================
object JsonWriteTest {

  // ---------------------------------------------------------------------------
  // from TimeTest
                                                    val localDate = "2022-03-16"
                                                    val localTime =             "11:38:40"

                                                    val instant        = "2022-03-16T15:38:40Z"
                                                    val  localDateTime = "2022-03-16T11:38:40"
                                                    val offsetDateTime = "2022-03-16T11:38:40-04:00"
                                                    val  zonedDateTime = "2022-03-16T11:38:40-04:00[America/Toronto]"

  // ===========================================================================
  def main(args: Array[String]): Unit = {
    check("bar", "bar".quote)
    check(true, "true")
    check(1,   "1")
    check(1.1, "1.1")

    // ---------------------------------------------------------------------------
    check(1  .toByte,  "1")
    check(1  .toShort, "1")
    check(1  .toLong,  "1")
    check(1.1.toFloat, "1.1")

    // ---------------------------------------------------------------------------
    check(BigInt(1)  , "1")
    check(BigDec(1.1), "1.1")

    // ---------------------------------------------------------------------------
    check( localDate    .parseLocalDate,       localDate    .quote)
    check( localTime    .parseLocalTime,       localTime    .quote)
    check( localDateTime.parseLocalDateTime,   localDateTime.quote)
    check(offsetDateTime.parseOffsetDateTime, offsetDateTime.quote)
    check( zonedDateTime.parseZonedDateTime,   zonedDateTime.quote)
    check( instant      .parseInstant,          instant     .quote)

    // ---------------------------------------------------------------------------
    check(gallia.byteBuffer("foo"), "base64:Zm9v".quote)//(_.binary)("Bin:|Zm9v|")

    // ---------------------------------------------------------------------------
    check(EnumValue("bar"), "bar".quote)

    // ---------------------------------------------------------------------------
    "JSON write: OK".p
  }

  // ===========================================================================
  private def check[T: WeakTypeTag](value: T, expected: String) = {
    val x = s"""{"f":"foo","g":${expected}}"""

    bobj("f" -> "foo", "g" -> value)
      .identity
      .formatCompactJson
      .assert(_ == x, _ -> x)
  }

}

// ===========================================================================
