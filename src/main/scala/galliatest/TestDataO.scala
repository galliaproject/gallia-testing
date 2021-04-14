package galliatest

import gallia._

// ===========================================================================
object TestDataO {
  val Default00  = bobj('g -> 1)
  val Default00a = bobj('g -> 1)
  val Default00b = bobj('g -> 2)

  val Default01  = bobj('f -> "foo", 'g -> 1)
  val Default01b = bobj('f -> "foo2", 'g -> 2)
  val Default01c = bobj('f -> "oof", 'g -> 1)

  val Default02  = bobj('f -> Seq("foo1", "foo2"), 'g -> 1)

  val Default03  = bobj('p -> Default01 , 'z -> true)

  val Default04 = bobj('p -> Seq(Default01, Default01b), 'z -> true)

  val Default06  = bobj('f1 -> "foo" , 'f2 -> "foo" , 'g -> 1)

  val Default09 = bobj('p -> Default01, 'z -> true, 'f -> "foo")

  val Default10 = bobj('f -> "", 'g -> 1, 'h -> "", 'p -> bobj('pf -> "", 'pg -> 2))

  val Default11 = bobj('f -> "foo")

  // ===========================================================================
  val Default13p  = aobj(cls('f.string_, 'g.int))(obj('f -> "foo", 'g -> 1))
  val Default13m  = aobj(cls('f.string_, 'g.int))(obj(             'g -> 1))

  val Default14p  = aobj(cls('f.strings_, 'g.int))(obj('f -> Seq("foo1", "foo2"), 'g -> 1))
  val Default14m  = aobj(cls('f.strings_, 'g.int))(obj(                           'g -> 1))

  // ===========================================================================
  val Default15p = aobj(
      cls('f.string_ , 'g.int , 'h.boolean))(
      obj('f -> "foo", 'g -> 1, 'h -> true))

  val Default15m = aobj(
      cls('f.string_, 'g.int , 'h.boolean))(
      obj(            'g -> 1, 'h -> true))

  // ---------------------------------------------------------------------------
  val Default16p = aobj(
      cls('f.strings_              , 'g.int , 'h.boolean))(
      obj('f -> Seq("foo1", "foo2"), 'g -> 1, 'h -> true))

  val Default16m = aobj(
      cls('f.strings_, 'g.int , 'h.boolean))(
      obj(             'g -> 1, 'h -> true))

}

// ===========================================================================
