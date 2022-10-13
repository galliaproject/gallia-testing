package galliatest.suites

import gallia._
import aptus._

// ===========================================================================
object RuntimeValidationTest extends gallia.testing.Suite {	  

  // ---------------------------------------------------------------------------
  private implicit class Obj___(o: Obj) {
  	def validate(c: Cls)(expected: Any) = // TODO: as part of suite
  		gallia.RuntimeValidation.validate(c)(o)
  		  .assert(
          _ == expected,
          _.str.swp -> expected.str.swp)
  }

  // ===========================================================================
  override def test() { import gallia.RuntimeValidation.ValErr
    val c = cls('f.ints , 'g.string)//FIXME: f must be required
    val o = obj('f -> Seq(1, 2, 3), 'g -> "a")
    
    //cls:
    //	f               	_Nes	_Int
    //	g               	_One	_String
    //{"f":[1,2,3],"g":"a"}
    
    // ---------------------------------------------------------------------------
    TestDataO.Default01.retain("g").check(TestDataO.Default00)
    
    // ---------------------------------------------------------------------------
    o.validate(c)(None)

    // ---------------------------------------------------------------------------    
    obj('f -> Seq(1, 2, 3), 'g -> new java.io.File("") )
        .validate(c) {
      Some(List(ValErr(19, 'g, None, "unrecognized type"))) }

    obj('f -> Seq(1, 2, 3))
        .validate(c) {
      Some(List(ValErr(11, 'g, None, "missing required field: g               	_String"))) }
        
    obj('f -> Seq(1, 2, 3), 'g -> 0)
        .validate(c) {
      Some(List(ValErr(17, 'g, None, "is _Int but should be _String"))) }
    obj('f -> Seq(1, 2, 3), 'g -> Seq("a", "b"))
        .validate(c) {
      Some(List(ValErr(13, 'g, None, "shouldn't be mult"))) }
    
    obj('f -> Seq(1, 2, 3), 'g -> Seq(0, 1))
        .validate(c) {
      Some(List(
          ValErr(13, 'g, None, "shouldn't be mult"),
          ValErr(17, 'g, None, "is _Int but should be _String"))) }
    
    obj('f -> Seq("a", "b", "c"), 'g -> "a")
        .validate(c) {
      Some(List(ValErr(17, 'f, None, "is _String but should be _Int"))) }
    
    obj('f -> 1, 'g -> "a")
        .validate(c) {
      Some(List(ValErr(12, 'f, None, "should be mult"))) }
 
    // ---------------------------------------------------------------------------
    obj('f -> Seq(1, 2, 3), 'g -> "a", 'h -> obj('hh -> true))
          .validate(c.add(        'h.boolean )) {
      Some(List(ValErr(14, 'h, None, "is obj but should be _Boolean"))) }
    
    obj('f -> Seq(1, 2, 3), 'g -> "a", 'h -> obj('hh -> "a"))
      .validate(c.add('h.cls('hh.boolean))) {
        Some(List(
          ValErr(16, 'h, None, ("nested errors", List(
            ValErr(15, 'h, Some(0), ("nested errors", List(
              ValErr(17, 'hh, None, "is _String but should be _Boolean"))))))))) }


    // ===========================================================================
    // enums

    obj("f" -> EnumValue("a")).validate(cls("f".enm("a", "b")))(None)

    obj("f" -> EnumValue("c")).validate(cls("f".enm("a", "b"))) {
      Some(List(ValErr(23, 'f, None, "invalid enum value: c"))) }

    obj("f" -> 3)
      .validate(cls("f".enm("a", "b"))) {
        Some(List(ValErr(21, 'f, None, "is _Int but should be _Enm(List(a, b))"))) }

    obj("f" -> EnumValue("a"))
      .validate(cls("f".string)) {
        Some(List(ValErr(22, 'f, None, "is _Enm but should be _String"))) }

  }
}

// ===========================================================================
