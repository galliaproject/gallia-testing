package galliatest.suites.single

import aptus._ // for .as.noneIf
import gallia._

// ===========================================================================
object CcMappingTest extends gallia.testing.Suite {
	import TestDataO.Default01
	import galliatest.TestMeta.{Foo, Bar}

	// ---------------------------------------------------------------------------
  override def test() {
    // manually would be: nest 'f under 'g, rename 'g |> 'f as 'a, then generate 'g |> 'A from 'a
    Default01   .transform(_.string('f)).using { s =>     Foo(s, s.toUpperCase)                               }.check(bobj('f ->     bobj('a -> "foo", 'A -> "FOO")                                                   , 'g -> 1))
    Default01   .transform(_.string('f)).using { s => Seq(Bar(s, s.toUpperCase, 1), Bar(s, s.toUpperCase, 2)) }.check(bobj('f -> Seq(bobj('a -> "foo", 'A -> "FOO", 'i -> 1), /*a*/bobj('a -> "foo", 'A -> "FOO", 'i -> 2)), 'g -> 1))
  
    Default01   .transform(_.string('f)).using { s => s.in.noneIf(_.startsWith("z")).map(s2 => Foo(s2, s2.toUpperCase)) }.check(
      aobj(
        cls('f  .cls_('a.string, 'A.string), 'g.int))(
        obj('f -> obj('a -> "foo", 'A -> "FOO"), 'g -> 1)))
  
    // will remove it
    Default01   .transform(_.string('f)).using { s => s.in.noneIf(_.startsWith("f")).map(s2 => Foo(s2, s2.toUpperCase)) }.check(
      aobj(
        cls('f  .cls_('a.string, 'A.string), 'g.int))(
        obj(                                 'g -> 1)) )
	}
	
}

// ===========================================================================
