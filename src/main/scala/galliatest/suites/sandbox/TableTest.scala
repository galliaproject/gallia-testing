package galliatest
package suites
package sandbox

import aptus._
import gallia._

// ===========================================================================
object TableTest {

  val file = "/data/test/test.tsv"

  // ===========================================================================
  def main(args: Array[String]): Unit = {

    {
      val c = cls("f".string, "g".int)
      file.stream(_.schema(c))._assert2(bobjs(
        bobj("f" -> "bar1", "g" -> 1),
        bobj("f" -> "bar2", "g" -> 2)))
    }

    // ---------------------------------------------------------------------------
    {
      val c = cls("f".string, "g".byte)

      file.stream(_.schema(c))._assert2(bobjs(
        bobj("f" -> "bar1", "g" -> 1.toByte),
        bobj("f" -> "bar2", "g" -> 2.toByte)))
    }

    // ---------------------------------------------------------------------------
    {
      val c = cls("f".enm("bar1", "bar2"), "g".byte)

      file.stream(_.schema(c))
        ._assert2(c.aobjs(
          obj("f" -> "bar1".e, "g" -> 1.toByte),
          obj("f" -> "bar2".e, "g" -> 2.toByte)))
    }

    // ---------------------------------------------------------------------------
    "table: OK".p
  }

}

// ===========================================================================
