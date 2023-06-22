package galliatesting0
package suites

import aptus._
import gallia._

// ===========================================================================
package object sandbox {
  val _Optional = true
  val _Multiple = true

  val _Required = false
  val _Single   = false

  // ===========================================================================
  implicit class EnumTestString_(u: String) { def e: EnumValue = EnumValue(u) }

  // ===========================================================================
  implicit class Obj220405111108(o: Obj) {
    def forceKey   (key: Key):        Any  = o._data.find  (_._1 == key).map(_._2).get

    // ---------------------------------------------------------------------------
    def debugObj(): Obj = toDebug(o)

    // ---------------------------------------------------------------------------
    def exactlyEquals(that: Obj): Boolean =
      o          == that &&
      o.debugObj == that.debugObj
  }

  // ===========================================================================
  implicit class ic220405111107(aobj: AObj) {
      def display2() = { aobj.identity.display2() }

      // ---------------------------------------------------------------------------
      def exactlyEquals(that: AObj): Boolean =
        aobj.c            == that.c &&
        aobj.o.exactlyEquals(that.o)

      // ---------------------------------------------------------------------------
      def formatSandbox =
        Seq(
          aobj.c         .formatDefault,
          aobj.o         .formatDefault,
          aobj.o.debugObj.formatDefault)
        .joinln

      // ---------------------------------------------------------------------------
      def _assert2(expected: AObj) = {
        assert(
          exactlyEquals(expected),
          Seq(
              expected.formatSandbox.sectionAllOff("expected:"),
              aobj    .formatSandbox.sectionAllOff("actual:"))
            .section) }

      // ---------------------------------------------------------------------------
      def _assert2(origin: AObj, expected: AObj) = {
        assert(
          aobj.c == expected.c,
          Seq(
              expected.c.formatDefault.sectionAllOff("expected:"),
              aobj    .c.formatDefault.sectionAllOff("actual:"),
              origin  .c.formatDefault.sectionAllOff("origin:"))
            .section )

        // ---------------------------------------------------------------------------
        assert(
          toDebug(aobj.o) == toDebug(expected.o),
          Seq(
            toDebug(expected.o).formatDefault.sectionAllOff("expected:"),
            toDebug(aobj    .o).formatDefault.sectionAllOff("actual:"),
            toDebug(origin  .o).formatDefault.sectionAllOff("origin:"))
            .section)

        // ---------------------------------------------------------------------------
        assert(
          aobj.o == expected.o,
          Seq(
              expected.o.formatDefault.sectionAllOff("expected:"),
              aobj    .o.formatDefault.sectionAllOff("actual:"),
              origin  .o.formatDefault.sectionAllOff("origin:"))
            .section)
      }

    }

    implicit class ic220512122102(aobj: AObjs) {
  //    def display2() = { aobj.identity.display2() }
  //
      // ---------------------------------------------------------------------------
      def exactlyEquals(that: AObjs): Boolean =
        aobj.c            == that.c &&
        aobj.z.toListAndTrash.zipSameSize(that.z.toListAndTrash).forall(x => x._1.exactlyEquals(x._2))

      // ---------------------------------------------------------------------------
      def formatSandbox =
        (Seq(aobj.c.formatDefault) ++
          aobj.z.toListAndTrash.map(_         .formatDefault) ++
          aobj.z.toListAndTrash.map(_.debugObj.formatDefault))
        .joinln

      // ---------------------------------------------------------------------------
      def _assert2(expected: AObjs) = {
        assert(
          exactlyEquals(expected),
          Seq(
              expected.formatSandbox.sectionAllOff("expected:"),
              aobj    .formatSandbox.sectionAllOff("actual:"))
            .section) }
    }

  // ===========================================================================
  implicit class ic220412163227(head: HeadO) {

      def checkCls(expected: Cls) =
        head
          ._forceResult
          .o
          .pipe(Cls.fromObj)
          .assert(_ == expected)

      // ===========================================================================
              def display2() = { head.display(); head._forceResult.o.debugObj.p }

              // ===========================================================================
              def _assert2              (expected: BObj) = { head._forceResult._assert2(expected.forceAObj) }
              def _assert2              (expected: AObj) = { head._forceResult._assert2(expected) }
              def _assert2(origin: AObj, expected: AObj) = { head._forceResult._assert2(origin, expected) }

              // ===========================================================================
              @aptus.fordevonly def _assert(expected: BObj): HeadO = _assert(expected.forceAObj)
              @aptus.fordevonly def _assert(expected: AObj): HeadO = head.tap { _._forceResult._assert2(expected) }
              @aptus.fordevonly def _fail  (any: Any): Unit = head.tap { x => util.Try(x._forceResult).failed.get.toString.assert(_.contains(any.toString)) }

              @aptus.fordevonly def _silentRun(): HeadO = head.tap { _._forceResult }
            }
            // ===========================================================================
            implicit class ic220512121955(head: HeadZ) {
              def display2() = { head.display(); head._forceResult.z.toListAndTrash.map(_.debugObj)                 .joinln.p }
              def display3() = { head.display(); head._forceResult.z.toListAndTrash.map { x =>
                x.formatPrettyJson ->
                x.debugObj.formatPrettyJson }.joinln.p }

              // ===========================================================================
              def _assert2              (expected: BObjs) = { head._forceResult._assert2(expected.forceAObjs) }
              def _assert2              (expected: AObjs) = { head._forceResult._assert2(expected) }
//              def _assert2(origin: AObj, expected: AObj) = { head._forceResult._assert2(origin, expected) }
//
//              // ===========================================================================
//              @aptus.fordevonly def _assert(expected: BObj): HeadZ = _assert(expected.forceAObj)
//              @aptus.fordevonly def _assert(expected: AObj): HeadZ = head.tap { _._forceResult._assert2(expected) }
//              @aptus.fordevonly def _fail  (any: Any): Unit = head.tap { x => util.Try(x._forceResult).failed.get.toString.assert(_.contains(any.toString)) }
//
//              @aptus.fordevonly def _silentRun(): HeadZ = head.tap { _._forceResult }
            }

  // ===========================================================================
  private[sandbox] def toDebug(o: Obj): Obj = o.modifyValuesRecursively { bsc => s"${bsc.getClass.getSimpleName}:|${bsc}|" }
}

// ===========================================================================
