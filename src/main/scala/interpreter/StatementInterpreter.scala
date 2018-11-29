package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}

object StatementInterpreter {
  /*
   * @param sParse The statement parse
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   */
  def apply(
    sParse: StatementParse,
    env: Environment
  ): Outcome[Vector[EnvironmentCommand], String] = {
    val id = sParse.identifier
    val typeExpression = sParse.typeExpression
    val objExpression = sParse.expression

    for {
      typeAsType <- TypeChecker.typeSpecificTypeChecker(typeExpression, env)
      tc <- TypeChecker.typeCheck(objExpression, typeAsType, env)
      evaluatedObject <- Evaluator(tc.objectFound, env)
    } yield {
      Vector(EnvironmentCommand(id.s, typeAsType, evaluatedObject))
    }
  }
}