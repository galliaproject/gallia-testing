package galliatest
package suites

import gallia._
import aptus._

// ===========================================================================
object EnumTest extends gallia.testing.Suite {

  override def test() { import sandbox._; TestDataO.Default01.noop(_.identity)

    val aobj1 = AObj(cls("f".string, "e".enm("a", "b")),      obj("f" -> "foo", "e" -> "a".e))
    val aobj2 = AObj(cls("f".string, "e".enm("a", "b")),      obj("f" -> "foo", "e" -> "b".e))
    val aobj3 = AObj(cls("f".string, "e".string),             obj("f" -> "foo", "e" -> "b"  ))
    val aobj4 = AObj(cls("f".string, "e".int),                obj("f" -> "foo", "e" ->  1   ))
    val aobj5 = AObj(cls("f".string, "e".enm("a", "b", "c")), obj("f" -> "foo", "e" -> "a".e))

    // ---------------------------------------------------------------------------
    aobj1.transform(_.enm("e")).using(identity)             ._assert(aobj1)
    aobj1.transform(_.enm("e")).using { _.stringValue.size }._assert(aobj4)

    // ---------------------------------------------------------------------------
    aobj1.transform(_.enm("e")).using { case EnumValue("a") =>           "b" ; case EnumValue("b") =>           "a"  }._assert(aobj3)
    aobj1.transform(_.enm("e")).using { case EnumValue("a") => EnumValue("b"); case EnumValue("b") => EnumValue("a") }._assert(aobj2)
    aobj1.transform(_.enm("e")).using { case EnumValue("a") =>            1  ; case EnumValue("b") =>            2   }._assert(aobj4)

    aobj1.transform(_.enm("e")).using { case EnumValue("a") => Seq("a".e, "b".e); case EnumValue("b") => Seq("b".e, "a".e) }._assert(
      AObj(cls("f".string, "e".enms("a", "b")),      obj("f" -> "foo", "e" -> Seq("a".e, "b".e))) )

    // ---------------------------------------------------------------------------
    aobj1.convert("e").toStr.convert("e").toEnum("a", "b", "c")                               ._assert(aobj5)
    aobj1.convert("e").toStr.convert("e").toEnum("a", "b", "a")                               ._fail("InvalidEnumStringValues")

    aobj1                                         .modifyEnumValuesFor("e").using(_ :+ "c".e) ._assert(aobj5)
    aobj1.identity.forEachKey("e").thn((x, k) => x.modifyEnumValuesFor( k) .using(_ :+ "c".e))._assert(aobj5)
    aobj1                                         .modifyEnumValuesFor("F").using(_ :+ "a".e)._fail("NoSuchField")
    aobj1                                         .modifyEnumValuesFor("f").using(_ :+ "a".e)._fail("NotAnEnumField")
    aobj1                                         .modifyEnumValuesFor("e").using(_ :+ "a".e)._fail("InvalidEnumStringValues")

    // ---------------------------------------------------------------------------
    assert(obj("f" -> "a".e).enumeratum[MyEnum]("f") == MyEnum.a)
    assert(obj("f" -> "a".e).enm               ("f") == EnumValue("a"))

    // ---------------------------------------------------------------------------
    assert(aobj1.forceEnm("e").stringValue == "a")

    // ---------------------------------------------------------------------------
    // whatever
    aobj1.transform("e").using(_.stringValue.toUpperCase)._assert(AObj(cls("f".string, "e".string),      obj("f" -> "foo", "e" -> "A")))

    // ---------------------------------------------------------------------------
    gallia.reflect.ReflectUtils.enumValueNames[MyEnum].assert(_ == List("a", "b"))

    // ---------------------------------------------------------------------------
    "enums: OK".p
  }
}

// ===========================================================================
sealed trait MyEnum extends enumeratum.EnumEntry

    object MyEnum extends enumeratum.Enum[MyEnum] {
      val values = findValues

      // ---------------------------------------------------------------------------
      case object a extends MyEnum
      case object b extends MyEnum
    }

  // ---------------------------------------------------------------------------
  sealed trait MyEnum2 extends enumeratum.EnumEntry

    object MyEnum2 extends enumeratum.Enum[MyEnum2] {
      val values = findValues

      // ---------------------------------------------------------------------------
      case object foo  extends MyEnum2
      case object foo2 extends MyEnum2
    }

// ===========================================================================