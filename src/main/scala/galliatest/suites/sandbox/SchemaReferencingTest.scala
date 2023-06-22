package galliatesting0
package suites
package sandbox

import gallia._
import aptus._

// ===========================================================================
object SchemaReferencingTest {

  val MySchema =
"""
{
  "fields": [{ "key": "f", "info": "ref:220428141309" }],
  "refs": [
    { "_id": "220428130045", "value": false },
    { "_id": "220428141309", "value": { "optional": false, "union": [{ "multiple": "ref:220428130045", "valueType": "ref:220428130046" }] } },
    { "_id": "220428130046", "value": "_String" } ]
}
"""

  // ---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {

    val c = cls("f".string)
    //c.formatPrettyJson.p
    //MetaObj2.clsFromString(MySchema).p
  }

}

// ===========================================================================
