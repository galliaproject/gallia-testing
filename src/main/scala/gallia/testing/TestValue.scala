package gallia.testing

import util._
import gallia._

// ===========================================================================
sealed trait TestValue { def isOk: Boolean = this == Ok }

  // ---------------------------------------------------------------------------
  case object Ok                        extends TestValue
  case class  Problem(value: Throwable) extends TestValue

// ===========================================================================
private object TestValue {
  private def problem(message: String): TestValue = Problem(new Throwable(message))

  // ===========================================================================
  def __check(u: HeadEnd, value: AObj) : TestValue = __check(u, value.c, value.u)
  def __check(u: HeadEnd, value: AObjs): TestValue = __check(u, value.c, value.z)

    // ---------------------------------------------------------------------------
    def __check[$Data](u: HeadEnd, expC: Cls, expD: $Data): TestValue =
      util.Try { u.run[$Data] } match {
        case util.Failure(f)   => problem(f.getMessage)
        case util.Success(res) =>
          res.either match {
            case Left (metaErrorResult)                                   => problem(s"210414125640:${metaErrorResult.formatDefault}")
            case Right(successResult) if (successResult.leavesCount != 1) => problem(s"210414125643:MultipleLeaves")
            case Right(successResult) =>
              val (meta, data) = successResult.pair

                   if (meta != expC) problem(s"\n\nexpected:\n${expC}\n\ngot:\n${meta}\n")
              else if (data != expD) problem(s"\n\nexpected:\n${expD}\n\ngot:\n${data}\n")
              else                   Ok } }

  // ===========================================================================
  def __metaError(end: gallia.heads.HeadEnd, markers: Seq[String]): TestValue =
    Try { end.runMetaOnly().either } match {
      case Failure(metaFailure)                                                                  => problem(s"210414113945:MetaFailure:${metaFailure.getMessage}")
      case Success(Right(metaSuccess))                                                           => problem("210414114600:ShouldHaveSucceeded")
      case Success(Left(metaErrorResult)) if (!metaErrorResult.containsAllErrorMarkers(markers)) => problem(s"210414114601:MissingErrorMarkers:${metaErrorResult.formatDefault}")
      case Success(Left(metaErrorResult))                                                        => Ok }

  // ===========================================================================
  def __dataError(end: gallia.heads.HeadEnd, markers: Seq[String]): TestValue =
    Try { end.runMetaOnly().either } match {
      case Failure(metaFailure)            => problem(s"210414113945:MetaFailure:${metaFailure.getMessage}")
      case Success(Left (metaErrorResult)) => problem(s"210414114439:MetaError:${metaErrorResult.formatDefault}")
      case Success(Right(metaSuccess))     => ___dataError(plan = metaSuccess.data, markers) }

    // ---------------------------------------------------------------------------
    def ___dataError(plan: ActionPlan, markers: Seq[String]): TestValue =
      Try { plan.atomPlan.naiveRun() } match {
        case util.Success(_)                                                             => problem("210414114600:ShouldHaveSucceeded")
        case util.Failure(dataError) if (!markers.forall(dataError.getMessage.contains)) => problem(s"210414114601:MissingErrorMarkers:${dataError.getMessage}")
        case util.Failure(dataError)                                                     => Ok }

}