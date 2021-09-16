package galliatest.suites.single

import gallia._
import aptus._
import gallia.vldt._Error.{Runtime => RuntimeError}

// ===========================================================================
object GenerateTest extends gallia.testing.Suite {

  // ===========================================================================
  override def test() {
import TestDataO._  
import galliatest.TestMeta._

    // ===========================================================================
//    val Default010 = aobjs(Default01, Default01b)
    // TODO:x ([Err((200701174017,TypeMismatch) - for p |> f: expected: _One:String, got: _Nes:String)],digraph default {
    if (false)
      Default04
        .generate('ff)
          .from(_.strings('p |> 'f))
            .using(_.map(_.toUpperCase))

    // ---------------------------------------------------------------------------
//Default03.boo('p).baa(_.string('f)).using(_.size)
//Default03.transform(_.obj('p)).using { _.stringA('f).apply(_.size) }.check()

    Default03
        .generate('h)
          .from(_.obj('p))
            .using(_.translate('f ~> 'F).using("foo" -> "oof").remove('g))
      .check(bobj('p -> bobj('f -> "foo", 'g -> 1), 'z -> true, 'h -> bobj('F -> "oof")))

   Default04
    .generate('p2)
      .from(_.objz('p))
        .using(_.rename('f ~> 'F))
      .check(bobj('p -> Seq(Default01, Default01b), 'z -> true, 'p2 -> Seq(bobj('F -> "foo", 'g -> 1), bobj('F -> "foo2", 'g -> 2))))

    // ===========================================================================
    Default03p
            .generate('h)
              .from(_.obj('p))
                .using(_.translate('f ~> 'F).using("foo" -> "oof").remove('g))
          .check(aobj(
            cls('p   .cls_('f.string, 'g.int   ), 'z.boolean, 'h .cls_ ('F.string)))(
            obj('p -> obj ('f -> "foo", 'g -> 1), 'z -> true, 'h -> obj('F -> "oof")) ))

// from article
aobj(
  cls('p   .cls_('f.string, 'g.int   ), 'z.boolean))(
  obj('p -> obj ('f -> "foo", 'g -> 1), 'z -> true) )
      .generate('h)
        .from(_.obj('p))
          .using {
            _ .translate('f ~> 'F).using("foo" -> "oof")
              .remove('g) }
    .check {
      aobj(
        cls('p   .cls_('f.string, 'g.int   ), 'z.boolean, 'h .cls_ ('F.string)))(
        obj('p -> obj ('f -> "foo", 'g -> 1), 'z -> true, 'h -> obj('F -> "oof")) ) }

        Default03m
            .generate('h)
              .from(_.obj('p))
                .using(_.translate('f ~> 'F).using("foo" -> "oof").remove('g))
          .check(aobj(
            cls('p   .cls_('f.string, 'g.int   ), 'z.boolean, 'h .cls_ ('F.string)))(
            obj(                                  'z -> true                      ) ))

        // ---------------------------------------------------------------------------
        Default03p
            .generate('h)
              .from(_.obj/*_*/('p))
                .using(/*_.map(*/_.translate('f ~> 'F).using("foo" -> "oof").remove('g)/*)*/)
          .check(aobj(
            cls('p   .cls_('f.string, 'g.int   ), 'z.boolean, 'h .cls_ ('F.string)))(
            obj('p -> obj ('f -> "foo", 'g -> 1), 'z -> true, 'h -> obj('F -> "oof")) ))

        Default03m
            .generate('h)
              .from(_.obj/*_*/('p))
                .using(/*_.map(*/_.translate('f ~> 'F).using("foo" -> "oof").remove('g)/*)*/)
          .check(aobj(
            cls('p   .cls_('f.string, 'g.int   ), 'z.boolean, 'h .cls_ ('F.string)))(
            obj(                                  'z -> true                      ) ))

    // ===========================================================================
    Default01
      .generate('f1, 'f2).from(_.string('f))
          .using { f => (
              f.underscore("1"),
              f.underscore("2")) }
        .check(bobj('f -> "foo", 'g -> 1, 'f1 -> "foo_1", 'f2 -> "foo_2"))


    // ===========================================================================
    Default01.generate('f2).from(_.string('f)).using(_.underscore)   .check(bobj('f -> "foo", 'g -> 1, 'f2 -> "foo_"))

    Default01.generate('f2).from('f)          .using(_ + "_2")       .check(bobj('f -> "foo", 'g -> 1, 'f2 -> "foo_2")) // uses  WV
    Default01.generate('f2).from('f)          .using(_.sizeString)   .check(bobj('f -> "foo", 'g -> 1, 'f2 -> 3))       // uses TWV[T] with T=Int        
    Default01.generate('f2).from('f)          .using(_.toString.size).check(bobj('f -> "foo", 'g -> 1, 'f2 -> 3))       // uses     T  with T=Int

    // ---------------------------------------------------------------------------
    // disallowed
    //    Default01
    //      .generate('f2)
    //        .from(
    //            _.explicit('f),
    //            _.explicit('g))
    //          .using { (f, g) =>
    //            f.toString.underscore(g.toString) }
    //        .check(bobj('f -> "foo", 'g -> 1, 'f2 -> "foo_1"))
    
    Default01
      .generate('f2)
        .from(
            _.string('f),
            _.int   ('g))
          .using { (f, g) =>
            f.underscore(g.str) }
        .check(bobj('f -> "foo", 'g -> 1, 'f2 -> "foo_1"))

    // these all use WV
    bobj('f -> "foo", 'g -> "bar").generate('f2).from('f, 'g).using { (f, g) => f + "_" + g }.check(bobj('f -> "foo", 'g -> "bar", 'f2 -> "foo_bar"))
    bobj('f -> 2,     'g -> 3)    .generate('f2).from('f, 'g).using { (f, g) => f * g }      .check(bobj('f -> 2,     'g -> 3,     'f2 -> 6))        
    bobj('f -> 2,     'g -> 3.5)  .generate('f2).from('f, 'g).using { (f, g) => f * g }      .dataError[RuntimeError.DifferingRuntimeType] // see 210811110423@design
    bobj('f -> 2,     'g -> 3.4)  .generate('f2).from('f, 'g).using { (f, g) => f * g }      .dataError[RuntimeError.DifferingRuntimeType] // see 210811110423@design
    bobj('f -> 3.4,   'g -> 2)    .generate('f2).from('f, 'g).using { (f, g) => f * g }      .check(bobj('f -> 3.4,   'g -> 2,     'f2 -> 6.8))

    Default01.generate('f2).from('f, 'g).using { (f, g) => f.sizeString    * g }.check(bobj('f -> "foo", 'g -> 1, 'f2 -> 3)) // uses TWV[T] with T=Int    
    Default01.generate('f2).from('f, 'g).using { (f, g) => f.toString.size * g }.check(bobj('f -> "foo", 'g -> 1, 'f2 -> 3)) // uses     T  with T=Int
  //Default01.generate('f2).from('f, 'g).using { (f, g) => f.toString.size + g }.check(bobj('f -> "foo", 'g -> 1, 'f2 -> 4)) // can't because scala allows: 3 + "foo" (bad?)   
    
    // ===========================================================================
    Default01
      .generate('f1, 'f2)
        .from(_.string('f))
          .using { s => (
              s.head.toString,
              s.tail) }
        .check(bobj('f -> "foo", 'g -> 1, 'f1 -> "f", 'f2 -> "oo"))

    Default01
      .generate('f1, 'f2)
        .from(_.string('f))
          .using { s => (
              s.headOption.map(_.toString),
              s.tail) }
        .check(
            bobj('f -> "foo", 'g -> 1, 'f1 -> "f" , 'f2 -> "oo")
          .toNonRequired('f1) )

    // ---------------------------------------------------------------------------
    def tmp4(x: AObj) =
        x.generate('f1, 'f2)
            .from(_.string_('f))
              .using { sOpt =>
                ( sOpt.map(_.head.str).getOrElse("."),
                  sOpt.map(_.tail    )) }

      tmp4(Default15p)
        .check(aobj(
            cls('f.string_ , 'g.int , 'h.boolean, 'f1.string, 'f2.string_))(
            obj('f -> "foo", 'g -> 1, 'h -> true, 'f1 -> "f", 'f2 -> "oo")) )

      tmp4(Default15m)
        .check(aobj(
            cls('f.string_ , 'g.int , 'h.boolean, 'f1.string, 'f2.string_))(
            obj(             'g -> 1, 'h -> true, 'f1 -> ".")) )

    // ---------------------------------------------------------------------------
    Default01
      .generate('ff)
        .from(_.string('f))
          .using(_.toUpperCase)
        .check(bobj('f -> "foo", 'g -> 1, 'ff -> "FOO"))

    Default03
      .generate('p |> 'z2)
        .from(_.boolean('z))
          .using(x => !x)
        .check(bobj('p -> bobj('f -> "foo", 'g -> 1, 'z2 -> false), 'z -> true))

    // ---------------------------------------------------------------------------
    Default03
      .generate('ff)
        .from(_.string('p |> 'f))
          .using(_.toUpperCase)
        .check(bobj('p -> bobj('f -> "foo", 'g -> 1), 'z -> true, 'ff -> "FOO"))

    Default03
      .generate('ff)
        .from(
            _.string('p |> 'f),
            _.int   ('p |> 'g))
          .using { (f, g) =>
              f.toUpperCase + (g + 1) }
        .check(bobj('p -> Default01, 'z -> true, 'ff -> "FOO2"))

    // ===========================================================================
    // generate - cc

    Default01
      .generate('h)
        .from(_.string('f))
          .using { s => Foo(s, s.toUpperCase) }
        .check(
            bobj('f -> "foo", 'g -> 1, 'h -> bobj('a -> "foo", 'A -> "FOO")))

    Default01
      .generate('h)
        .from(_.string('f))
          .using { s => Seq(
              Foo(s.append("1"), s.toUpperCase.append("1")),
              Foo(s.append("2"), s.toUpperCase.append("2"))) }
        .check(
            bobj('f -> "foo", 'g -> 1, 'h -> Seq(
              bobj('a -> "foo1", 'A -> "FOO1"),
              bobj('a -> "foo2", 'A -> "FOO2") )))

    Default01
      .generate('h1, 'h2)
        .from(_.string('f))
          .using { s => (
              Foo(s.append("1"), s.toUpperCase.append("1")),
              Foo(s.append("2"), s.toUpperCase.append("2"))) }
        .check(bobj('f -> "foo", 'g -> 1,
            'h1 -> bobj('a -> "foo1", 'A -> "FOO1"),
            'h2 -> bobj('a -> "foo2", 'A -> "FOO2")) )

    Default01
      .generate('baz1, 'h2)
        .from(_.string('f))
          .using { s => (
              Baz1(
                  a = s.append("1"),
                  A = s.toUpperCase.append("1"),
                  Qux(s.size)),
              "hh2") }
        .check(bobj('f -> "foo", 'g -> 1,
            'baz1 -> bobj('a -> "foo1", 'A -> "FOO1", 'q -> bobj('i -> 3)),
            'h2 -> "hh2"))

    Default01
      .generate('baz2, 'h2)
        .from(_.string('f))
          .using { s => (
              Baz2(
                  a = s.append("1"),
                  A = s.toUpperCase.append("1"),
                  Some(Qux(s.size))),
              "hh2") }
        .check(aobj(
         cls('f.string,   'g.int , 'baz2   .cls('a.string   , 'A.string   , 'q.cls_('i.int)), 'h2.string))(
         obj('f -> "foo", 'g -> 1, 'baz2 -> obj('a -> "foo1", 'A -> "FOO1", 'q -> obj('i -> 3)), 'h2 -> "hh2")) )

    Default01
      .generate('baz2, 'h2)
        .from(_.string('f))
          .using { s => (
              Baz2(
                  a = s.append("1"),
                  A = s.toUpperCase.append("1"),
                  None),
              "hh2") }
      .check(aobj(
         cls('f.string,   'g.int , 'baz2   .cls('a.string   , 'A.string   , 'q.cls_('i.int)), 'h2.string))(
         obj('f -> "foo", 'g -> 1, 'baz2 -> obj('a -> "foo1", 'A -> "FOO1"                 ), 'h2 -> "hh2")) )

    Default01
      .generate('baz3, 'h2)
        .from(_.string('f))
          .using { s => (
              Baz3(
                  a = s.append("1"),
                  A = s.toUpperCase.append("1"),
                  Seq(Qux(s.size - 1), Qux(s.size + 1))),
              "hh2") }
      .check(
          bobj('f -> "foo", 'g -> 1,
              'baz3 -> bobj(
                  'a -> "foo1",
                  'A -> "FOO1",
                  'q -> Seq(
                      bobj('i -> 2),
                      bobj('i -> 4))),
              'h2   -> "hh2"))

    Default01
      .generate('baz3, 'h2)
        .from(_.string('f))
          .using { s => (
              Baz4(
                  a = s.append("1"),
                  A = s.toUpperCase.append("1"),
                  None),
              "hh2") }
      .check(
        aobj(
           cls('f.string,   'g.int , 'baz3   .cls('a.string   , 'A.string   , 'q.clss_('i.int)), 'h2.string))(
           obj('f -> "foo", 'g -> 1, 'baz3 -> obj('a -> "foo1", 'A -> "FOO1"                  ), 'h2 -> "hh2")) )

    // ===========================================================================
    Default01
      .generate('f2)
        .from(_.string('f))
          .using(_.toUpperCase)
        .check(bobj('f -> "foo", 'g -> 1, 'f2 -> "FOO"))

    Default01
      .generate('f2)
        .from(_.string('f))
          .using { s => Seq(Foo(s, s.toUpperCase)) }
        .check(bobj('f -> "foo", 'g -> 1, 'f2 -> Seq(bobj('a -> "foo", 'A -> "FOO"))))

    // ===========================================================================
    // generate

    Default03
      .generate('p |> 'z2)
        .from(_.boolean('z))
          .using(x => !x)
        .check(bobj('p -> bobj('f -> "foo", 'g -> 1, 'z2 -> false), 'z -> true))

    Default03
      .generate('g2)
        .from(_.int('p |> 'g))
          .using(_ + 1)
        .check(bobj('p -> bobj('f -> "foo", 'g -> 1), 'z -> true, 'g2 -> 2))

    Default03
      .generate     ('p |> 'g2)
        .from(_.int('p |> 'g))
          .using(_ + 1)
        .check(bobj('p -> bobj('f -> "foo", 'g -> 1, 'g2 -> 2), 'z -> true))

    Default01
      .generate('h)
        .from(_.string('f))
          .using { s => Foo(s, s.toUpperCase) }
        .check(
            bobj('f -> "foo", 'g -> 1, 'h -> bobj('a -> "foo", 'A -> "FOO")))

    // ===========================================================================
    Default01
      .generate('h1, 'h2)
        .from(_.string('f))
          .using { s => (
              Foo(s.append("1"), s.toUpperCase.append("1")),
              Foo(s.append("2"), s.toUpperCase.append("2"))) }
        .check(bobj('f -> "foo", 'g -> 1,
            'h1 -> bobj('a -> "foo1", 'A -> "FOO1"),
            'h2 -> bobj('a -> "foo2", 'A -> "FOO2")) )

//{"f":"foo","g":1,"h1":{"a":"foo1","A":"FOO1"},"h2":{"a":"foo2","A":"FOO2"}}
//{"f":"foo","g":1,"h1":"Foo(foo1,FOO1)","h2":"Foo(foo2,FOO2)"}
//

    // ===========================================================================
    Default03.generate('x).from(_.obj('p)).using(_.rename('f ~> 'F))
      .check(aobj(
          cls('p   .cls ('f.string, 'g.int   ), 'z.boolean, 'x   .cls ('F.string, 'g.int   )))(
          obj('p -> obj ('f -> "foo", 'g -> 1), 'z -> true, 'x -> obj ('F -> "foo", 'g -> 1)) ))

    Default03p.generate('x).from(_.obj('p)).using(_.rename('f ~> 'F))
      .check(aobj(
          cls('p   .cls_('f.string, 'g.int   ), 'z.boolean, 'x   .cls_('F.string, 'g.int   )))(
          obj('p -> obj ('f -> "foo", 'g -> 1), 'z -> true, 'x -> obj ('F -> "foo", 'g -> 1)) ))

    Default03m.generate('x).from(_.obj('p)).using(_.rename('f ~> 'F))
      .check(aobj(
          cls('p   .cls_('f.string, 'g.int   ), 'z.boolean, 'x   .cls_('F.string, 'g.int   )))(
          obj(                                  'z -> true) ))

    // ===========================================================================
    bobj('a -> 2, 'b -> 3)
      .generate('ab)
        .from(_.int('a), _.int('b))
          .using(_ * _)
      .check(bobj('a -> 2, 'b -> 3, 'ab -> 6))

    // ---------------------------------------------------------------------------     
    bobj('a -> 2, 'b -> 3)
      .generate('ab)
        .from('a, 'b)
          .using { (a, b) => a * b }
      .check(bobj('a -> 2, 'b -> 3, 'ab -> 6))
      
    // ---------------------------------------------------------------------------     
    bobj('a -> 2, 'b -> 3, 'c -> 4)
      .generate('abc)
        .from('a, 'b, 'c)
          .using { (a, b, c) => a * b * c }
      .check(bobj('a -> 2, 'b -> 3, 'c -> 4, 'abc -> 24))
      
    // ---------------------------------------------------------------------------
    bobj('a -> 3.1, 'b -> 2) // ok because double comes first (see 210817130604)
        .generate('ab)
          .from('a, 'b)
            .using { (a, b) => a * b }
        .check(bobj('a -> 3.1, 'b -> 2, 'ab -> 6.2))
        
      bobj('a -> 2, 'b -> 3.1) // not so
        .generate('ab)
          .from('a, 'b)
            .using { (a, b) => a * b }
        .dataError[RuntimeError.DifferingRuntimeType]      
  
      bobj('a -> 3.1, 'b -> 2)
        .generate('ab)
          .from('a, 'b)
            .using { (a, _) => a * 2 }
        .check(bobj('a -> 3.1, 'b -> 2, 'ab -> 6.2))      
  }

}

// ===========================================================================
