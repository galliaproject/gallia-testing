package gallia.testing

import aptus._
import gallia.CallSite

// ===========================================================================
case class TestResults(values: Seq[TestResult]) {
    def suiteNames: Seq[String] = values.map(_.suiteName).distinct
    
    def isMultipleSuites: Boolean = suiteNames.size > 1
    
    def counts: (Int, Int) = values.partition(_.value.isOk).thn { case (oks, kos) => (oks.size, kos.size) }
    
    def problem: Boolean = counts.thn { case (oks, kos) => (oks == 0 || kos > 0) }
    
    // ---------------------------------------------------------------------------
    def groupedBySuiteName[T](f: (String, TestResults) => T): Seq[T] =
      values
        .map { test => test.suiteName -> test }
        .groupByKeyWithListMap        
        .map { case (name, subset) =>
          f(name, TestResults(subset)) }
        .toList
  }
  
  // ===========================================================================
  object TestResults {    
    private val tests = cross.MutList[TestResult]()

    // ---------------------------------------------------------------------------
    private[testing] def add(value: TestResult) { tests += value }

    // ---------------------------------------------------------------------------
    private[testing] def printResults() {
      val tmp = tests.toList
      tests.clear()      
      val results = TestResults(tmp)      

      // ---------------------------------------------------------------------------
      results
        .values
        .map { test =>
          test.value match {
            case Ok                 => s"OK: ${if (results.isMultipleSuites) test.suiteName.tab else ""}${test.location}"
            case Problem(throwable) => s"KO: ${if (results.isMultipleSuites) test.suiteName.tab else ""}${test.location}\n${throwable.getMessage.sectionAllOff}" } }
        .foreach(println)

      // ---------------------------------------------------------------------------
      if (results.isMultipleSuites) {
        println(s"\n${results.suiteNames.size} suites:")
        results
          .groupedBySuiteName { (name, subset) =>
            val (oksCount, kosCount) = subset.counts    
            print(s"\t${(name.colon).padRight(32, ' ')}")
              print(s"\t${oksCount.str.padLeft(3, ' ')} OK,")
              print(s"\t${kosCount.str.padLeft(3, ' ')} KO")
              println }
      }

      // ---------------------------------------------------------------------------  
      val (oksCount, kosCount) = results.counts
      println("\ntotals:")
        println(s"\t${oksCount.str.padLeft(5, ' ')} OK")
        println(s"\t${kosCount.str.padLeft(5, ' ')} KO")

      // ---------------------------------------------------------------------------
      if (results.problem) {
        throw new IllegalStateException("Houston, we have a problem.") }
    }

  }

// ===========================================================================