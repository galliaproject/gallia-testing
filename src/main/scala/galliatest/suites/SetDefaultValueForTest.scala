package galliatest.suites

import gallia._

// ===========================================================================
object SetDefaultValueForTest extends gallia.testing.Suite {

  override def test() {
    test1(in = TestDataO.Default13m)
    test2(inM = TestDataO.Default15m, inP = TestDataO.Default15p)
  }
  
  // ===========================================================================
  private def test1(in: AObj) {  
    in.setDefaultFor('f)                      .asValue("default").check(bobj('f -> "default", 'g -> 1))
    in.setDefaultFor(_.ifOptionalString)      .asValue("default").check(bobj('f -> "default", 'g -> 1))
    in.setDefaultFor(_.ifType[Option[String]]).asValue("default").check(bobj('f -> "default", 'g -> 1)) // bit awkward...
  }
  
  // ===========================================================================
  private def test2(inM: AObj, inP: AObj) {    
    inM.setDefaultFor('f ~> 'F    ).asValue("-").check(bobj('F -> "-", 'g -> 1, 'h -> true))
    inM.setDefaultFor('f ~> 'F, 'g).asValue("-").metaError(vldt.ErrorId.TypeMismatch)

    inM.forKey(_.firstKey).zen(_.setDefaultFor(_).asValue("-")).check(bobj('f -> "-", 'g -> 1, 'h -> true))
    inM.setDefaultFor(_.firstKey  ).asValue("-").check(bobj('f -> "-", 'g -> 1, 'h -> true))
    inP.setDefaultFor('f          ).asValue("-").check(bobj('f -> "foo", 'g -> 1, 'h -> true))
    inP.setDefaultFor('f ~> 'F    ).asValue("-").check(bobj('F -> "foo", 'g -> 1, 'h -> true))

    inM.setDefaultFor(_.allButLast).asValue("-").metaError(vldt.ErrorId.TypeMismatch)
    inM.setDefaultFor(
        'f,
        'g ~> 'G,
        'p |> 'h,
        'p |> 'i ~> 'I)
      .asValue("-") // compiles
  }

}

// ===========================================================================
