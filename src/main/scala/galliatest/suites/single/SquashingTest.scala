package galliatesting0
package suites
package single

import aptus._
import gallia._
import gallia.vldt.ErrorId ._

// ===========================================================================
object SquashingTest extends gallia.testing.Suite with gallia.testing.More {
  import TestDataO._
  import TestDataS._
  
  // ---------------------------------------------------------------------------
  private def liftValue[T: scala.reflect.runtime.universe.WeakTypeTag](a: T): HeadV[T] = heads.Head.inputV(a)

  // ===========================================================================
  private val present1 = aobj(
    cls("f".string_, "g".int))(
    obj("f"-> "foo", "g"-> 1) )

  // ---------------------------------------------------------------------------
  private val missing1 = aobj(
    cls("f".string_, "g".int))(
    obj(             "g"-> 1) )

  // ---------------------------------------------------------------------------
  private val present2 = aobj(
    cls("f".strings_              , "g".int))(
    obj("f"-> Seq("foo1", "foo2") , "g"-> 1) )

  // ---------------------------------------------------------------------------
  private val missing2 = aobj(
    cls("f".strings_, "g".int))(
    obj(              "g"-> 1) )

  // ===========================================================================
  override def test() {
    Default06a.fuse  (_.string("f1"), _.int("g")).as(_tmp).using((f1, g) => f1.size + g).int(_tmp).check(5) // equivalent but longer (220620111230)
    Default06a.squash(_.string("f1"), _.int("g"))         .using((f1, g) => f1.size + g)          .check(5)
    Default06a.squashUnsafe(o => o.string("f1").size + o.int("g"))                                .check(5)

    Default03.transform(_.entity("p")).using(_.squash(_.string("f"), _.int("g")).using((f, g) => f.size + g)            ).check(bobj('p -> 4, 'z -> true))
    Default03.transform(_.entity("p")).using(_.squash(_.string("f"), _.int("g")).using((f, g) => f.size + g).mapV(_ + 1)).check(bobj('p -> 5, 'z -> true))
    Default03.transform(_.entity("p")).using(_.squashUnsafe(o => o.string("f").size + o.int("g")           )            ).check(bobj('p -> 4, 'z -> true))
    Default03.transform(_.entity("p")).using(_.squashUnsafe(o => o.string("f").size + o.int("g")           ).mapV(_ + 1)).check(bobj('p -> 5, 'z -> true))

    liftValue(3    ).mapV(_ + 1).check(4)
    liftValue("foo").mapV(_.size).check(3)
    liftValue("foo").mapV(_.size).mapV(_ + 1).check(4)

    liftValue(Seq(1, 2, 3)).mapV     (_.map(_ + 1)).check(Seq(2, 3, 4))
    liftValue(Seq(1, 2, 3)).mapVs[Int, Int](_ + 1) .check(Seq(2, 3, 4)) // worth keeping?
    liftValue(Seq(1, 2, 3)).mapV (_.reduceLeft(_ + _)).check(6)

    {
      val (v1, v2) = (liftValue("foo"), liftValue(1))

      v1.combine(v2).using((x, y) => x.append((y + 1).toString)).check("foo2")
    }

  // ===========================================================================
  // grab/squash
  Default01.squashUnsafe(o => o.string("f").size + o.int("g")).check(4)

  Default01.squash(_.int("g")).using(identity).check(1)
  Default01.grab(_.int("g"))                  .check(1)
  Default01.forceInt("g").assert(_ == 1)

  Default52.squashUnsafe  (_.map(_.int("g")).sum).check(4)

  Default52.squash(_.int("g")).using(_.sum).check(4)
  Default56.squash(_.string("f1"), _.string('f2)).using { _.map { case (f1, f2) => f1.size * f2.size }.sum }.check(48)

  Default52.squash(_.int("g")).using(identity).check(Seq(1, 1, 2))
  Default52.grab(_.int("g"))                  .check(Seq(1, 1, 2))
  if (false) /*FIXME*/ Default52.grab(_.ints("g"))                  .check(Seq(Seq(1) /*, ...*/))
  Default52.forceInts("g").assert(_ == Seq(1, 1, 2))
  Default52.ints     ("g").check(      Seq(1, 1, 2))

  aobjs(
      aobj(
        cls('f.string  , 'g.int_))(
        obj("f"-> "foo1", "g"-> 1)),
      aobj(
        cls('f.string  , 'g.int_))(
        obj("f"-> "foo2")) )
    .ints_("g").check(Some(Seq(1)))


    // accessors
    assert(4 == Default01.forceSquashUnsafe(o => o.string("f").size + o.int("g")))
    assert(1 == Default01.forceInt         ("g"))

    //u.transform("f").using(_.size); u.fuse

    // ---------------------------------------------------------------------------
    // accessors

  //Default01.squash(_.string("f")).using(identity).check("foo")
    Default01 .grab(_.string ("f"))                .check("foo")
    Default15p.grab(_.string_("f")).check(Some("foo"))
    Default15m.grab(_.string_("f")).check(None)

    Default01     .string("f").check("foo")
    Default01.forceString("f").check("foo")

           present1.string("f").metaError(TypeMismatch)
    throws(present1.forceString("f"))

    present1.     string_("f").check(Some("foo"))
    present1.forceString_("f").check(Some("foo") )

           missing1.string("f").metaError(TypeMismatch)
    throws(missing1.forceString("f"))

    missing1.     string_("f").check(None)
    missing1.forceString_("f").assert(_.isEmpty )

    Default02.forceStrings ("f").check(     Seq("foo1", "foo2"))
  //Default02.strings_9("f").ttest(Some(Seq("foo1", "foo2")))

    present2.forceStrings_("f").check(Some(Seq("foo1", "foo2")))
    missing2.forceStrings_("f").check(None)

    Default01.int("f").metaError(TypeMismatch)

    // ===========================================================================
//TODO: int vs ints inconsistency - also vs ooo
    val GrabIn  = bobj("a"-> Seq(bobj("h"-> 1), bobj("h"-> 2)))
    val GrabOut = bobj("a"-> Seq(1,2))

  GrabIn.transform(_.entities("a")).using(_.grab(_.int("h"))).check(GrabOut)
  GrabIn.transform(_.entities("a")).using(_      .ints("h") ).check(GrabOut)
if(false) GrabIn.transform(_.entities("a")).using(_.grab("h")).check(GrabOut) // FIXME?
  }


}

// ===========================================================================
