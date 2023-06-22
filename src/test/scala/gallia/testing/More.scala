package gallia
package testing

// ===========================================================================
trait More {
    protected[testing] def test()

    // ===========================================================================
    def main(args: Array[String]): Unit = { apply() }

      // ---------------------------------------------------------------------------
      def apply() {
        ActualTestOpt = Some(false)

        More.ignoreStdErr()
          test()
        More.resetStdErr()

        gallia.testing.TestResults.printResults()
        ActualTestOpt = None
      }
  }

  // ===========================================================================
  object More {

    def testSuites(
        suite1: gallia.testing.Suite with gallia.testing.More,
        more  : gallia.testing.Suite with gallia.testing.More*) {
      ActualTestOpt = Some(false)

      ignoreStdErr()
        (suite1 +: more)
          .foreach(_.test())
      resetStdErr()

      gallia.testing.TestResults.printResults()

      ActualTestOpt = None
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

  }

// ===========================================================================
