package galliatesting0
package suites
package single

import gallia._
import gallia.vldt.ErrorId

// ===========================================================================
object ForXTest extends gallia.testing.Suite with gallia.testing.More {
	import TestDataO.{Default01, Default00, Default03, Default06a}
	
	// ---------------------------------------------------------------------------
  override def test() {	  
    val in1 = bobj('f1 -> 8.0, 'f2 -> 4.0, 'f3 -> 2.0)
  
      in1.pipe                        (          _.square('f1)).check(bobj('f1 -> 64.0, 'f2 ->  4.0, 'f3 -> 2.0))
      in1.thn                         (          _.square('f1)).check(bobj('f1 -> 64.0, 'f2 ->  4.0, 'f3 -> 2.0))
      in1.forKey    (_.firstKey  ).thn((u, k) => u.square(k)  ).check(bobj('f1 -> 64.0, 'f2 ->  4.0, 'f3 -> 2.0))
      in1.forKey    (_.firstKey  ).thn(          _ square _   ).check(bobj('f1 -> 64.0, 'f2 ->  4.0, 'f3 -> 2.0))
      in1.forKey    ('f1)         .thn(          _ square _   ).check(bobj('f1 -> 64.0, 'f2 ->  4.0, 'f3 -> 2.0)) // for good measure
  
      in1.forEachKey('f1, 'f2)    .thn(_ square _).check(bobj('f1 -> 64.0, 'f2 -> 16.0, 'f3 -> 2.0))
      in1.forEachKey(_.allButLast).thn(_ square _).check(bobj('f1 -> 64.0, 'f2 -> 16.0, 'f3 -> 2.0))
      in1.forEachKey(_.allKeys   ).thn(_ square _).check(bobj('f1 -> 64.0, 'f2 -> 16.0, 'f3 -> 4.0))
  
      in1.forKey    (_.firstKey  ).thn(_ double _).check(8.0)
      in1.forKey    (_.firstKey  ).thn { (u, k) => u.transform(_.double(k)).using(_ * 2.5) }.check(bobj('f1 -> 20.0, 'f2 -> 4.0, 'f3 -> 2.0))

//FIXME + for recursive, if, for key opt

    // ---------------------------------------------------------------------------
    val in2 = bobj('p -> bobj('f1 -> 1, 'f2 -> 2), 'f3 -> 3, 'z -> true)

      in2.forEachPath(_.customLeafPaths(_.init))                            .thn(          _ increment _ ).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 3), 'f3 -> 4, 'z -> true))
      in2.forEachPath(_.customLeafPaths(_.init))                            .thn((u, p) => u.increment(p)).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 3), 'f3 -> 4, 'z -> true))
    //in2.forEachPath(_.customAllPaths (_.init))                            .thn(k => _.increment(k)).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 3), 'f3 -> 4, 'z -> true))


in2.forPath(_.explicit('p |> 'f1)).thn(_ renameToUpperCase _).check(bobj('p -> bobj('F1 -> 1, 'f2 -> 2), 'f3 -> 3, 'z -> true))

in2.forEachPath(_.leafPaths).thn(_ renameToUpperCase _).check(bobj('p -> bobj('F1 -> 1, 'F2 -> 2), 'F3 -> 3, 'Z -> true))

in2.forLeafPaths(_ renameToUpperCase _).check(bobj('p -> bobj('F1 -> 1, 'F2 -> 2), 'F3 -> 3, 'Z -> true))
in2.forAllPaths (_ renameToUpperCase _).check(bobj('P -> bobj('F1 -> 1, 'F2 -> 2), 'F3 -> 3, 'Z -> true))

in2.forAllPaths(            _ renameToUpperCase _)  .check(bobj('P -> bobj('F1 -> 1, 'F2 -> 2), 'F3 -> 3, 'Z -> true))
in2.forAllPaths { (u, p) => u.renameToUpperCase(p) }.check(bobj('P -> bobj('F1 -> 1, 'F2 -> 2), 'F3 -> 3, 'Z -> true))

      in2.forEachPath(_.fullCustomPaths(_.filter3(_.isOneInt)))            .thn(_ increment _).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 3), 'f3 -> 4, 'z -> true))
      in2.forEachPath(_.fullCustomPaths(_.filter5(_.isOneInt)))            .thn(_ increment _).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 3), 'f3 -> 4, 'z -> true))

      in2.forEachPath(_.fullCustomPaths(_.filter3(_.skey.startsWith("f")))).thn(_ increment _).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 3), 'f3 -> 4, 'z -> true))
      in2.forEachPath(_.fullCustomPaths(_.filter5(_.skey.startsWith("f")))).thn(_ increment _).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 3), 'f3 -> 4, 'z -> true))

      // if key/path/field matches
      in2.forEachPath(_.fullCustomPaths(_.filter5(_.key == 'f1)))                .thn(_ increment _).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 2), 'f3 -> 3, 'z -> true))
      in2.forEachPath(_.fullCustomPaths(_.filter5(x => x.key == 'f1 && x.isOne))).thn(_ increment _).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 2), 'f3 -> 3, 'z -> true))
      in2.forEachPath(_.fullCustomPaths(_.filter5(_.path == ('p |> 'f1))))       .thn(_ increment _).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 2), 'f3 -> 3, 'z -> true))

      in2.forEachPath(_.fullCustomPaths(_.filter5(_.isOneInt)))                  .thn(_ increment _).check(bobj('p -> bobj('f1 -> 2, 'f2 -> 3), 'f3 -> 4, 'z -> true))

    // ---------------------------------------------------------------------------
    // optional
    aobj(cls('f1.double_, 'g.int))(obj('f1 -> 8.0, 'g -> 1)).forKey(_.firstKey).thn(_ double_ _).check(Some(8.0))
    aobj(cls('f1.double_, 'g.int))(obj(            'g -> 1)).forKey(_.firstKey).thn(_ double_ _).check(None)
    aobj(cls('f1.double_, 'g.int))(obj(            'g -> 1)).forKey(_.firstKey).thn(_ double  _).metaError(ErrorId.TypeMismatch)

    aobj(cls('f1.double_, 'g.int))(obj('f1 -> 8.0, 'g -> 1)).forKey(_.explicit('F1)).thn(_ square _).metaError(ErrorId.NoSuchField)
    aobj(cls('f1.double_, 'g.int))(obj('f1 -> 8.0, 'g -> 1)).forKey(_.firstKey     ).thn(_ square _).check(aobj(cls('f1.double_, 'g.int))(obj('f1 -> 64.0, 'g -> 1)))
    aobj(cls('f1.double_, 'g.int))(obj(            'g -> 1)).forKey(_.firstKey     ).thn(_ square _).check(aobj(cls('f1.double_, 'g.int))(obj(             'g -> 1)))

  bobj('p -> in1).forPath    (_.explicit(           'p |> 'f2)).thn(_ square _).check(bobj('p -> bobj('f1 ->  8.0, 'f2 ->  16.0, 'f3 -> 2.0)))
  bobj('p -> in1).forEachPath(_.explicit('p |> 'f1, 'p |> 'f2)).thn(_ square _).check(bobj('p -> bobj('f1 -> 64.0, 'f2 ->  16.0, 'f3 -> 2.0)))

  val in3 = bobjs(bobj('f1 -> 8.0, 'f2 -> 4.0), bobj('f1 -> -8.0, 'f2 -> -4.0))

    in3.pipe                     (     _.square('f1)).check(bobjs(bobj('f1 -> 64.0, 'f2 -> 4.0), bobj('f1 -> 64.0, 'f2 -> -4.0)))
    in3.thn                      (     _.square('f1)).check(bobjs(bobj('f1 -> 64.0, 'f2 -> 4.0), bobj('f1 -> 64.0, 'f2 -> -4.0)))
    in3.forKey (_.firstKey  ).thn(_ square _).check(bobjs(bobj('f1 -> 64.0, 'f2 -> 4.0), bobj('f1 -> 64.0, 'f2 -> -4.0)))


  val A  =       bobj(            'f -> Seq(1, 2), 'z -> true)
  @deprecated
  val Ab =       bobj('z -> true, 'f -> Seq(1, 2))/* TODO: reorder keys (swap) */
  val B  = bobjs(bobj('f -> 1, 'z -> true), bobj('f -> 2, 'z -> true))

  in1       .double ('f1) .check(    8.0       )
  in1.thn (_.double ('f1)).check(    8.0       )
  in3       .doubles('f1) .check(Seq(8.0, -8.0))
  in3.thn (_.doubles('f1)).check(Seq(8.0, -8.0))
  A   .thn (_ flattenBy('f))             .check(B)
  B   .thn (_.group('f).by('z).force.one).check(Ab)

  A.forKey(_.firstKey).thn(_ flattenBy _)              .check(B)
  B.forKey(_.firstKey).thn(_.group(_).by('z).force.one).check(Ab) /* TODO: t201007093053 - forKeyPair */

    // ---------------------------------------------------------------------------
    Default01.rename('f).to('F)        .check(bobj('F -> "foo", 'g -> 1))
  //Default01.rename(_.firstKey)                .to('F).check(bobj('F -> "foo", 'g -> 1))
    Default01.forKey(_.firstKey).thn(_.rename(_).to('F)).check(bobj('F -> "foo", 'g -> 1))
    Default00.renameSoleKey('G).check(bobj('G -> 1))


    Default01.forKey(_.firstKey).thn(_ retain _).check(bobj('f -> "foo"))
    Default01.retain(_.firstKey).check(bobj('f -> "foo"))
  //Default03.retain(_.explicit('p |> 'f ~> 'F)).check(bobj('p -> bobj('F -> "foo"), 'z -> true)) // TODO: re-allow?
    Default03.retain(_.explicit('p |> 'f      )).check(bobj('p -> bobj('f -> "foo")))

    Default01.remove(_.firstKey).check(bobj('g -> 1))
    Default01.remove(_.firstKey).check(bobj('g -> 1))

    Default06a.remove(_.firstKey   ).check(bobj(               'f2 -> "foo2", 'g -> 1))
    Default06a.remove(_.allButFirst).check(bobj('f1 -> "foo1"))

    // ===========================================================================
    aobj(cls('f.string_, 'g.string_, 'h.boolean))(obj('h -> true))                         .setDefaultFor('f, 'g).asValue("-") .check(bobj('f -> "-", 'g -> "-", 'h -> true))
    aobj(cls('f.string_, 'g.string_, 'h.boolean))(obj('h -> true)).forEachKey('f, 'g).thn(_.setDefaultFor(_)     .asValue("-")).check(bobj('f -> "-", 'g -> "-", 'h -> true))

    //Default13m.forKey(_.string_('f)).thn(_.transform(_).using(x => x))
  }
}

// ===========================================================================
