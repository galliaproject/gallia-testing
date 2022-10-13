package galliatest
package suites
package sandbox
package json

import aptus._
import gallia._

// ===========================================================================
object JsonTest3 {

  val j1 =
    """
        {
          "f": "foo",
          "g": 1.1,
          "p": {
            "pg": 2.2
          }
        }
      """

  val j2 = j1.replace("1.1", "1")

  val j3 =
    """
        {
          "f": "foo",
          "g": 1
        }
      """

  // ---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    check(j1, cls("f".string, "g".double, "p".cls("pg".double)))(
        obj("f" -> "String:|foo|", "g" -> "Double:|1.1|", "p" -> obj("pg" -> "Double:|2.2|")))

    check(j1, cls("f".string, "g".double, "p".cls("pg".float )))(
        obj("f" -> "String:|foo|", "g" -> "Double:|1.1|", "p" -> obj("pg" -> "Float:|2.2|")))

    check(j1, cls("f".string, "g".float, "p".cls("pg".float)))(
        obj("f" -> "String:|foo|", "g" -> "Float:|1.1|", "p" -> obj("pg" -> "Float:|2.2|")))

    check(j3, cls("f".string, "g".int))(
        obj("f" -> "String:|foo|", "g" -> "Integer:|1|"))

    check(j2, cls("f".string, "g".int, "p".cls("pg".double)))(
        obj("f" -> "String:|foo|", "g" -> "Integer:|1|", "p" -> obj("pg" -> "Double:|2.2|")))

    check(j2, cls("f".string, "g".short, "p".cls("pg".float)))(
        obj("f" -> "String:|foo|", "g" -> "Short:|1|", "p" -> obj("pg" -> "Float:|2.2|")))

    check(j2, cls("f".string, "g".int, "p".cls("pg".float)))(
        obj("f" -> "String:|foo|", "g" -> "Integer:|1|", "p" -> obj("pg" -> "Float:|2.2|")))

    // ---------------------------------------------------------------------------
    "json3: OK".p
  }
  
  // ===========================================================================
  private def check(j: String, cls: Cls)(expected: Obj) = {
    j
      .read(_.schema(cls))
      ._forceResult.o.debugObj
      .assert(_ == expected,
        x => "\n" + x.formatDefault.newline + expected.formatDefault.newline) }

}

// ===========================================================================
