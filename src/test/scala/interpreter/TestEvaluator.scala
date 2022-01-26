package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

class TestEvaluator extends FlatSpec {
  def assetFunctionWorkedAndReturnedResult(
    attempt: Outcome[Evaluator.ApplyFunctionAttemptResult, String],
    result: NewMapObject
  ): Unit = {
    attempt match {
      case Success(Evaluator.AbleToApplyFunction(output)) => assert(output == result)
      case Success(x) => fail(s"Unable to apply function $x")
      case Failure(f) => fail(s"Apply function failed: $f")
    }
  }

  "isCommandFunc " should " work properly on ranges" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc(0), NewMapO.rangeT(10), Environment.Base)
    assetFunctionWorkedAndReturnedResult(result, Index(1))
  }

  "it " should " work properly on count" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc(0), CountT, Environment.Base)
    assetFunctionWorkedAndReturnedResult(result, Index(1))
  }

  "it " should " work properly on TypeT, and return false" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc(1), TypeT(0), Environment.Base)
    assetFunctionWorkedAndReturnedResult(result, Index(0))
  }

  "it " should " fail TypeT when the wrong level is produced" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc(1), TypeT(0), Environment.Base)
    assert(result.isFailure)
  }
}