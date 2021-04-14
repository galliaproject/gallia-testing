package gallia

import aptus._
import util.Try

// ===========================================================================
package object testing {
  type HeadEnd    = gallia.heads.HeadEnd
  type ActionPlan = gallia.plans.ActionPlan

  // ===========================================================================
  def testSuites(suite1: Suite, more: Suite*) {
    ignoreStdErr()
    
    (suite1 +: more).foreach(_.test())
    
    TestResult.printResults()
  }

  // ===========================================================================
  def ignoreStdErr() {
    System.setErr(
      new java.io.PrintStream(
        new java.io.OutputStream() { 
          def write(x: Int) {} })) }

  // ===========================================================================
  private[testing] def _toNonRequired(value: AObj, keys: Seq[KeyW]): AObj =
    value
      .copy(c = keys
      .map(_.value)
      .foldLeft(value.c)(_ toNonRequired _))
  
}

// ===========================================================================  