package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

class TestEvaluator extends FlatSpec {
  def assertFunctionWorkedAndReturnedResult(
    attempt: Outcome[UntaggedObject, String],
    result: UntaggedObject
  ): Unit = {
    attempt match {
      case Success(output) => assert(output == result)
      case Failure(f) => fail(s"Apply function failed: $f")
    }
  }

  val env = (new EnvironmentInterpreter()).env

  def mapConfig = MapConfig(CommandOutput, SimpleFunction)

  "isCommandFunc " should " work properly on ranges" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, env.typeSystem.typeToUntaggedObject(IndexT(UIndex(10))), env)
    assertFunctionWorkedAndReturnedResult(result, UIndex(1))
  }

  it should " work properly on count" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, env.typeSystem.typeToUntaggedObject(CountT), env)
    assertFunctionWorkedAndReturnedResult(result, UIndex(1))
  }

  it should " work properly on TypeT" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, env.typeSystem.typeToUntaggedObject(TypeT), env)
    assertFunctionWorkedAndReturnedResult(result, UIndex(1))
  }
}