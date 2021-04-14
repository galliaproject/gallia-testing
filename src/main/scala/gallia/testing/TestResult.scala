package gallia.testing

import aptus._
import gallia.CallSite

// ===========================================================================
case class TestResult(
      suiteName: String, // eg "MergingTest"
      origin   : CallSite,
      value    : TestValue){
    def location: String =
      origin
        .sub
        .map(_.location)
        .getOrElse("unknown-location")
  }
  
  // ===========================================================================
  object TestResult {  
    private val results = cross.MutList[TestResult]()
    
    // ---------------------------------------------------------------------------
    private[testing] def add(value: TestResult) { results += value }
    
    // ---------------------------------------------------------------------------
    private[testing] def printResults() {
      val list = results.toList
      results.clear()

      val multipleSuites: Boolean = list.map(_.suiteName).distinct.size > 1
      
      // ---------------------------------------------------------------------------
      list
        .map { test =>
          test.value match {
            case Ok                 => s"OK: ${if (multipleSuites) test.suiteName.tab else ""}${test.location}"
            case Problem(throwable) => s"KO: ${if (multipleSuites) test.suiteName.tab else ""}${test.location}\n${throwable.getMessage.sectionAllOff}" } }
        .foreach(println)
  
      // ---------------------------------------------------------------------------
      val (oks, kos) = list.partition(_.value.isOk)
      println
      println(s"${oks.size} OK")
      println(s"${kos.size} KO")
      
      // ---------------------------------------------------------------------------
      if (oks.size ==0 || kos.size > 0) {
        gallia.illegalState("Houston, we have a problem.") }
    }
  }

// ===========================================================================