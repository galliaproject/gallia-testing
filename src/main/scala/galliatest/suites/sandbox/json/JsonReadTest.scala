package galliatest
package suites
package sandbox
package json

import gallia._
import aptus._

// ===========================================================================
object JsonReadTest {

  def main(args: Array[String]): Unit = {
    """{"value": true  }""".read(_.schema("value".boolean))._assert2(bobj("value" -> true ))

    """{"value": "foo" }""".read(_.schema("value".string))           ._assert2(bobj("value" -> "foo"))
    """{"value": "foo" }""".read(_.schema("value".enm("foo", "oof")))._assert2(aobj("value".enm("foo", "oof"))(obj("value" -> EnumValue("foo"))))

    """{"value": 1 }""".read(_.schema("value".int   ))._assert2(bobj("value" -> 1))
    """{"value": 1 }""".read(_.schema("value".byte  ))._assert2(bobj("value" -> 1.toByte ))
    """{"value": 1 }""".read(_.schema("value".short ))._assert2(bobj("value" -> 1.toShort))
    """{"value": 1 }""".read(_.schema("value".long  ))._assert2(bobj("value" -> 1.toLong ))
    """{"value": 1 }""".read(_.schema("value".bigInt))._assert2(bobj("value" -> BigInt(1) ))

    """{"value": 1.1 }""".read(_.schema("value".double))._assert2(bobj("value" -> 1.1))
    """{"value": 1.1 }""".read(_.schema("value".float ))._assert2(bobj("value" -> 1.1.toFloat))
    """{"value": 1.1 }""".read(_.schema("value".bigDec))._assert2(bobj("value" -> BigDec(1.1)  ))

                      // ---------------------------------------------------------------------------
                      val ms = "1,647,447,105,888"
                      val  s = "1,647,447,105" // max int is: 2,147,483,647
                      val  d = "19,067"

                      // ---------------------------------------------------------------------------
                      val localDate = "2022-03-16"
                      val localTime =             "11:38:40"

                      val instant        = "2022-03-16T15:38:40Z"
                      val instant2       = "2022-03-16T16:11:45.888Z"
                      val  localDateTime = "2022-03-16T11:38:40"
                      val  localDateTime2= "2022-03-16T12:11:45"

                      val offsetDateTime = "2022-03-16T11:38:40-04:00"
                      val  zonedDateTime = "2022-03-16T11:38:40-04:00[America/Toronto]"

import sandbox._
    s"""{"value": ${localDate    .quote} }""".read(_.schema("value".localDate    ))._assert2(bobj("value" -> localDate    .parseLocalDate))
    s"""{"value": ${localDateTime.quote} }""".read(_.schema("value".localDateTime))._assert2(bobj("value" -> localDateTime.parseLocalDateTime))
    s"""{"value": ${instant      .quote} }""".read(_.schema("value".instant      ))._assert2(bobj("value" -> instant      .parseInstant))

    s"""{"value": ${d .removeIfApplicable(",")} }""".read(_.schema("value".localDate    ))._assert2(bobj("value" -> localDate     .parseLocalDate))
    s"""{"value": ${s .removeIfApplicable(",")} }""".read(_.schema("value".localDateTime))._assert2(bobj("value" -> localDateTime2.parseLocalDateTime))
    s"""{"value": ${ms.removeIfApplicable(",")} }""".read(_.schema("value".instant      ))._assert2(bobj("value" -> instant2      .parseInstant))

    s"""{"value": ${     localTime.quote} }""".read(_.schema("value".     localTime))._assert2(bobj("value" ->      localTime.     parseLocalTime))
    s"""{"value": ${offsetDateTime.quote} }""".read(_.schema("value".offsetDateTime))._assert2(bobj("value" -> offsetDateTime.parseOffsetDateTime))
    s"""{"value": ${ zonedDateTime.quote} }""".read(_.schema("value". zonedDateTime))._assert2(bobj("value" ->  zonedDateTime. parseZonedDateTime))


    //TODO: binary

    "json read test: ok".p
  }

}

// ===========================================================================