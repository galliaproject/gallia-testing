package gallia

// ===========================================================================
package object testing {
  type HeadEnd    = gallia.heads.HeadEnd
  type ActionPlan = gallia.plans.ActionPlan

  // ===========================================================================
  def testSuites(suite1: Suite, more: Suite*) {
    ignoreStdErr()
      (suite1 +: more).foreach(_.test())
    resetStdErr()

    TestResults.printResults()
  }

  // ===========================================================================
  private val OriginalErr: java.io.PrintStream = System.err
  
    // ---------------------------------------------------------------------------
    def ignoreStdErr() {      
      System.setErr(
        new java.io.PrintStream(
          new java.io.OutputStream() {
            def write(ignore: Int) {} })) }

    // ---------------------------------------------------------------------------
    def resetStdErr() { System.setErr(OriginalErr) }
    
  // ===========================================================================
  private[testing] def _toOptional(value: AObj, keys: Seq[KeyW]): AObj =
    value
      .copy(c = keys
      .map(_.value)
      .foldLeft(value.c)(_ toOptional _))

}

// ===========================================================================
