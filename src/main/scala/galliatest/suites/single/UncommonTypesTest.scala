package galliatest
package suites
package single

import aptus._
import gallia._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}

// ===========================================================================
object UncommonTypesTest extends gallia.testing.Suite {

  override def test() {
    bobj('value -> new java.lang.Long(3)).noop(_.identity)

    // ---------------------------------------------------------------------------
    bobj('value ->       BigInt    (3)).noop(_.identity)
    bobj('value ->       BigDecimal(3)).noop(_.identity)
    bobj('value -> scala.BigDecimal(3)).noop(_.identity)

    // ---------------------------------------------------------------------------
    bobj('value -> "2021-01-08"         .parseLocalDate)    .noop(_.identity) // {"value":{"year":2021,"month":1,"day":8}} - does it work th eother way around?
    bobj('value -> "2021-01-08T01:02:03".parseLocalDateTime).noop(_.identity) // {"value":{"date":{"year":2021,"month":1,"day":8},"time":{"hour":1,"minute":2,"second":3,"nano":0}}}

    // ---------------------------------------------------------------------------    
    checkValue(           "foo" )(_.string, classOf[String])
    checkValue(byteBuffer("foo"))(_.binary, classOf[java.nio.ByteBuffer])
  }

  // ===========================================================================
  private def checkValue[T: WeakTypeTag](value: T)(f: String => Fld, klass: Class[_]) = { // TODO: as part of suite
      val x = bobj("value" -> value).forceAObj  
        x.c.check(cls(f("value")))
        x.o.check(obj(  "value" -> value))
        x.o.pipe(checkValueType(klass)) }
  
    // ---------------------------------------------------------------------------
    private def checkValueType(klass: Class[_])(o: Obj) { // TODO: as part of suite
  	  o .entries
  	    .force.one
  	    ._2
  	    .getClass
  	    .assert(klass.isAssignableFrom, x => (x, klass)) }	

}

// ===========================================================================
