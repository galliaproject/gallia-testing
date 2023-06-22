package galliatesting0
package suites

import gallia._

// ===========================================================================
object CustomTest extends gallia.testing.Suite with gallia.testing.More {
	import TestDataO._

	// ---------------------------------------------------------------------------
  override def test(): Unit = {
    Default01.customU2U(
        meta = _.addField("z".boolean),
        data = _.addEntry("z", true))
      .check(bobj("f" -> "foo", "g" -> 1, "z" -> true))

    class CustomObjToObj extends ObjToObj {
        def meta(c: Cls) = c.addField("z".boolean)
        def data(o: Obj) = o.addEntry("z", true) }
      Default01.custom(new CustomObjToObj).check(bobj("f" -> "foo", "g" -> 1, "z" -> true))
  }

}

// ===========================================================================
