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

  //def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)

  val env = (new EnvironmentInterpreter()).env

  def mapConfig = MapConfig(CommandOutput, SimpleFunction)

  "isCommandFunc " should " work properly on ranges" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, UType(IndexT(10)), env)
    assertFunctionWorkedAndReturnedResult(result, UIndex(1))
  }

  it should " work properly on count" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, UType(CountT), env)
    assertFunctionWorkedAndReturnedResult(result, UIndex(1))
  }

  it should " work properly on TypeT, and return false" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, UType(TypeT), env)
    assertFunctionWorkedAndReturnedResult(result, UIndex(0))
  }

  /*it should " work properly on DataTypeT, and return false" in {
    val result = Evaluator.applyFunctionAttempt(IsCommandFunc, DataTypeT(Vector.empty), env)
    assertFunctionWorkedAndReturnedResult(result, Index(1))
  }*/

  // TODO: remove this when IsSimpleFunction is eliminated
  // TODO - IsSimpleFunction should not exist!!
  /*"IsSimpleFunction" should "work properly" in {
    assertFunctionWorkedAndReturnedResult(
      Evaluator.applyFunctionAttempt(TaggedObject(
        IsSimpleFunction,
        MapT(AnyT, IndexT(2), mapConfig)
      ), TypeT, env),
      Index(0)
    )

    assertFunctionWorkedAndReturnedResult(
      Evaluator.applyFunctionAttempt(IsSimpleFunction, UMap(Vector.empty), env),
      Index(1)
    )

    assertFunctionWorkedAndReturnedResult(
      Evaluator.applyFunctionAttempt(TaggedObject(
        IsSimpleFunction,
        MapT(AnyT, IndexT(2), mapConfig)
      ), TaggedObject(UMap(Vector.empty), MapT(IndexT(10), IndexT(10), MapConfig(CommandOutput, BasicMap))), env),
      Index(1)
    )

    assertFunctionWorkedAndReturnedResult(
      Evaluator.applyFunctionAttempt(TaggedObject(
        IsSimpleFunction,
        MapT(AnyT, IndexT(2), mapConfig)
      ), TaggedObject(UMap(Vector.empty), MapT(IndexT(10), IndexT(10), MapConfig(CommandOutput, FullFunction))), env),
      Index(0)
    )
  }*/
}