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
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, Index(10), Environment.Base)
    assetFunctionWorkedAndReturnedResult(result, Index(1))
  }

  "it " should " work properly on count" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, CountT, Environment.Base)
    assetFunctionWorkedAndReturnedResult(result, Index(1))
  }

  "it " should " work properly on TypeT, and return false" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, TypeT, Environment.Base)
    assetFunctionWorkedAndReturnedResult(result, Index(0))
  }

  // TODO: remove this when IsSimpleFunction is eliminated
  "IsSimpleFunction" should "work properly" in {
    assetFunctionWorkedAndReturnedResult(
      Evaluator.applyFunctionAttempt(IsSimpleFunction, TypeT, Environment.Base),
      Index(0)
    )

    assetFunctionWorkedAndReturnedResult(
      Evaluator.applyFunctionAttempt(IsSimpleFunction, MapInstance(Vector.empty, MapT(Index(10), Index(10), CommandOutput, SimpleFunction)), Environment.Base),
      Index(1)
    )

    assetFunctionWorkedAndReturnedResult(
      Evaluator.applyFunctionAttempt(IsSimpleFunction, MapInstance(Vector.empty, MapT(Index(10), Index(10), CommandOutput, BasicMap)), Environment.Base),
      Index(1)
    )

    assetFunctionWorkedAndReturnedResult(
      Evaluator.applyFunctionAttempt(IsSimpleFunction, MapInstance(Vector.empty, MapT(Index(10), Index(10), CommandOutput, FullFunction)), Environment.Base),
      Index(0)
    )
  }
}