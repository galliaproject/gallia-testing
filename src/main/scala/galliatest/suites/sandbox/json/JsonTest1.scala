package galliatest
package suites
package sandbox
package json

import aptus._

import gallia._
import gallia.data.json.GsonToObj

// ===========================================================================
object JsonTest1 { private val InputDateString = "2022-03-10"
  private val InputDateEpoch  = 19061

  private val ExpectedDate = "2022-03-11".parseLocalDate

  // ===========================================================================
  def main(args: Array[String]): Unit = {
    {
    	bobj("value" -> BigInt(3))           .forceAObj//.p
    	//bobj("value" -> BigInt(3).bigInteger).forceAObj.p --> not allowed?
    	
      """{"f": "foo"}""".pipe(GsonToObj.fromObjectString).assert(_.exactlyEquals(obj("f" -> "foo")))
      """{"f": "1"}"""  .pipe(GsonToObj.fromObjectString).assert(_.exactlyEquals(obj("f" -> "1")))

      """{"f": "1"}""".read()                         ._assert2(bobj("f" -> "1"))
      """{"f": "1"}""".read(_.schema(cls("f".string)))._assert2(bobj("f" -> "1"))
    }

    // ---------------------------------------------------------------------------
    jsonTest2("1".quote, _.bigInt)(
        BigInt(-1)) {
          _.transform(_.bigInt("value")).using(-_) }

    jsonTest2("3.14".quote, _.bigDec)(
        BigDec(-3.14)) {
          _.transform(_.bigDec("value")).using(-_) }        

    // ---------------------------------------------------------------------------
    jsonTest2(InputDateString.quote, _.localDate)(
        ExpectedDate) {
          _.transform(_.localDate("value")).using(_.plusDays(1)) }

    jsonTest2(InputDateEpoch, _.int)(
        ExpectedDate) {
        _.transform(_.int("value")).using(_.toLocalDate.plusDays(1)) }        

    bobj("value" -> InputDateString)
        .transform(_.string("value")).using(_.parseLocalDate.plusDays(1).formatIso)
      ._assert2(bobj("value" -> "2022-03-11"))

    // ---------------------------------------------------------------------------
    // time
    """{"value": "1980-01-01"}""".read(_.schema("value".string))   ._assert2(aobj(cls("value".string))   (obj("value" -> "1980-01-01")))
    """{"value": "1980-01-01"}""".read(_.schema("value".localDate))._assert2(aobj(cls("value".localDate))(obj("value" -> "1980-01-01".parseLocalDate)))

    // ===========================================================================
    // binary
    // TODO: case x: ByteBuffer => x.array.toBase64.prepend("base64:").pipe(Gson.toJsonTree)

    // ===========================================================================
    "json1: OK".p
  }

  // ===========================================================================
  def jsonTest2[T: WTT]
        (jsonValue: Any, interpretation: String => Fld)
        (expectedValue: T)
        (g: HeadO => HeadO) =
      jsonTest1(expectedValue, jsonValue)(interpretation)(g)

    // ---------------------------------------------------------------------------
    def jsonTest1[T: WTT]
        (expectedValue: T, jsonValue: Any)
        (f: String => Fld)
        (g: HeadO => HeadO) = {
      s"""{"value": ${jsonValue.toString}}"""
        .read(_.schema(cls("value".pipe(f))))
        .pipe(g)
        ._assert2(expected =
          bobj("value" -> expectedValue).forceAObj) }
}

// ===========================================================================
