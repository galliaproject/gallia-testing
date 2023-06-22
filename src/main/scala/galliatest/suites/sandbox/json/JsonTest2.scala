package galliatesting0
package suites
package sandbox
package json

import aptus._
import gallia._
import galliatesting0.suites.sandbox.ic220405111107

// ===========================================================================
object JsonTest2 {

  def main(args: Array[String]): Unit = {         
    check("1.1")(_.double)("Double :|1.1|")
    
    check("1")  (_.int)   ("Integer:|1|")  
    
    check("1")  (_.byte)  ("Byte   :|1|")
    check("1")  (_.short) ("Short  :|1|")
    check("1")  (_.long)  ("Long   :|1|")
    check("1.1")(_.float) ("Float  :|1.1|")

    checkFail("1.1")(_.byte) ("Byte :|1|") //assertion failed: 1.1
    checkFail("1.1")(_.short)("Short:|1|") //assertion failed: 1.1
    checkFail("1.1")(_.int)  ("Int  :|1|") //assertion failed: 1.1
    checkFail("1.1")(_.long) ("Long :|1|") //assertion failed: 1.1

    check("1"  .quote)(_.bigInt) ("BigInt    :|1|")
    check("1"        )(_.bigInt) ("BigInt    :|1|")
    check("1.1".quote)(_.bigDec) ("BigDecimal:|1.1|")    
    check("1.1"      )(_.bigDec) ("BigDecimal:|1.1|")    

    // ---------------------------------------------------------------------------
    checkTemporal("2022-03-16")(_.localDate) ("LocalDate")
    checkTemporal("11:38:40"  )(_.localTime) ("LocalTime")
    
    checkTemporal("2022-03-16T15:38:40Z")                      (_. instant      )("Instant")       
    checkTemporal("2022-03-16T11:38:40")                       (_. localDateTime)( "LocalDateTime")
    checkTemporal("2022-03-16T11:38:40-04:00")                 (_.offsetDateTime)("OffsetDateTime")
    checkTemporal("2022-03-16T11:38:40-04:00[America/Toronto]")(_. zonedDateTime)( "ZonedDateTime")

    check("19,067"           .remove(","))(_.localDate)    ("LocalDate    :|2022-03-16|")
    check("1,647,447,105"    .remove(","))(_.localDateTime)("LocalDateTime:|2022-03-16T12:11:45|")
    check("1,647,447,105,888".remove(","))(_.instant)      ("Instant      :|2022-03-16T16:11:45.888Z|")

    check("2022-03-16T11:38:40".quote)(_.localDateTime) ("LocalDateTime:|2022-03-16T11:38:40|") // with T
    check("2022-03-16 11:38:40".quote)(_.localDateTime) ("LocalDateTime:|2022-03-16T11:38:40|") // without T

    // ---------------------------------------------------------------------------
    //check("base64:Zm9v".quote)(_.binary)("Bin:|Zm9v|")

    // ---------------------------------------------------------------------------
    check("a".quote)(_.enm("a", "b"))("EnumValue:|a|")

    // ---------------------------------------------------------------------------    
    "json2: OK".p
  }
  
  // ===========================================================================
  private def checkTemporal(value: String)(f: String => Fld)(klass: String) =
    check(value.quote)(f) (s"${klass}:|${value}|")

  // ---------------------------------------------------------------------------
  private def check(jsonValue: String)(f: String => Fld)(expected: String) = {
    common(jsonValue)(f)
      ._assert2(bobj("g" -> expected.replace(" ", "")).forceAObj) }

  // ---------------------------------------------------------------------------
  private def checkFail(jsonValue: String)(f: String => Fld)(expected: String) = {
    util.Try(common(jsonValue)(f)).assert(_.isFailure) }

  // ---------------------------------------------------------------------------
  private def common(jsonValue: String)(f: String => Fld): AObj =
    s"""{"g": ${jsonValue}}"""
      .read(_.schema(f("g")))
      ._forceResult.o.pipe(toDebug)
      .pipe(AObj(Cls.oneString("g"), _))

}

// ===========================================================================
