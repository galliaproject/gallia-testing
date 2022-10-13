package galliatest
package suites

import gallia._
import aptus._

// ===========================================================================
object MetaSchemaTest extends gallia.testing.Suite {

  override def test() { TestDataO.Default01.noop(_.identity)
    val input    = cls("p".cls("f".string, "g".int), "h".boolean)
    val expected = cls("p".cls("F".string, "g".int), "h".boolean)

    // ---------------------------------------------------------------------------
    input
      .metaAObj
      .transformSomeEntities("fields").matching("key" -> "p").using {
          _.transformAllEntities("info" |> "union").using { // only one
            _.transformEntity("valueType").using {
              _.transformString("fields" |> "key").using {
                case "f" => "F"; case x => x } } } }
          ._forceResult
          .o
          .pipe  (Cls.fromObj)
          .assert(_ == expected)

    // ---------------------------------------------------------------------------
    "metaschema: OK".p
  }

}

// ===========================================================================