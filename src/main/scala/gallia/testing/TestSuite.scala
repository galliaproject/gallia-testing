package gallia.testing


import gallia._

// ===========================================================================
trait Suite {
  private val name = getClass.getSimpleName.stripSuffix("$")

  // ---------------------------------------------------------------------------
  protected[testing] def test() // TODO: t210413103408 - annotation-based rather (extens sbt-testing directly?)

  // ===========================================================================
  def main(args: Array[String]): Unit = { apply() }

    // ---------------------------------------------------------------------------
    def apply() {
      ignoreStdErr()
      test()
      TestResult.printResults()
    }

  // ===========================================================================
  implicit class BObjs__(u: BObjs) { def noop(f: HeadS => HeadS) = { f(u).check(u) } }
  implicit class AObjs__(u: AObjs) { def noop(f: HeadS => HeadS) = { f(u).check(u) } }
  implicit class AObj__ (u: AObj ) { def noop(f: HeadO => HeadO) = { f(u).check(u) } }
  implicit class BObj__ (u: BObj)  { def noop(f: HeadO => HeadO) = { f(u).check(u) }
    def toNonRequired(key1: KeyW, more: KeyW*): AObj = testing._toNonRequired(u.forceAObj, (key1 +: more)) }

  // ===========================================================================
  protected trait Head__[$Data] {
    protected def end: HeadEnd

    // ===========================================================================
    def metaError(pair: (String, String)) { _metaError(pair._1, pair._2) }
    def dataError(pair: (String, String)) { _dataError(pair._1, pair._2) }

    // ---------------------------------------------------------------------------
    def metaError[$Error <: vldt._Error: WTT] { _metaError(node[$Error].leaf.inScopeName) }
    def dataError[$Error <: vldt._Error: WTT] { _dataError(node[$Error].leaf.inScopeName) }

    // ===========================================================================
    private[Suite] def _metaError(markers: String*) { addResult(TestValue.__metaError(end, markers)) }
    private[Suite] def _dataError(markers: String*) { addResult(TestValue.__dataError(end, markers)) }
  }

  // ===========================================================================
  implicit class HeadO__(u: HeadO) extends Head__[Obj] {
    protected def end = u.end()

    // ===========================================================================
    def noop (implicit in: BObj) = check(in)

    // ---------------------------------------------------------------------------
    def check(expected: BObj) { check(expected.forceAObj) }
    def check(expected: AObj) { addResult(TestValue.__check(end, expected)) }
  }

  // ===========================================================================
  implicit class HeadS__(u: HeadS) extends Head__[Objs] {
    protected def end = u.end()

    // ===========================================================================
    def noop (implicit in: BObjs) = check(in)

    // ---------------------------------------------------------------------------
    def check(value1: BObj, more: BObj*)       { check(BObjs(value1 +: more)) }
    def check(implicit expected: BObjs)        { check(expected.forceAObjs) }
    def check(c: Cls)(value1: Obj, more: Obj*) { check(aobjs(c)((value1 +: more):_*)) }
    def check(expected: AObjs)                 { addResult(TestValue.__check(end, expected)) }
  }

  // ===========================================================================
  private def addResult(value: TestValue) { TestResult add TestResult(name, CallSite.generate(), value) }
}

// ===========================================================================
