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
    sParse: EnvStatementParse,
    env: Environment
  ): Outcome[Vector[EnvironmentCommand], String] = {
    sParse match {
      case FullStatementParse(_, id, typeExpression, objExpression) => {
        for {
          typeAsType <- TypeChecker.typeSpecificTypeChecker(typeExpression, env, 0)
          tc <- TypeChecker.typeCheck(objExpression, ExplicitlyTyped(typeAsType), env, 0)
          evaluatedObject <- Evaluator(tc, env)
        } yield {
          Vector(FullEnvironmentCommand(id.s, NewMapObjectWithType(evaluatedObject, ExplicitlyTyped(typeAsType))))
        }
      }
      case InferredTypeStatementParse(_, id, objExpression) => {
        for {
          tc <- TypeChecker.typeCheck(objExpression, NewMapTypeInfo.init, env, 0)
          evaluatedObject <- Evaluator(tc, env)
        } yield {
          Vector(FullEnvironmentCommand(id.s, NewMapObjectWithType(evaluatedObject, tc.nTypeInfo)))
        }
      }
      case ExpressionOnlyStatementParse(exp) => {
        for {
          tc <- TypeChecker.typeCheck(exp, NewMapTypeInfo.init, env, 0)
          evaluatedObject <- Evaluator(tc, env)
        } yield {
          Vector(ExpOnlyEnvironmentCommand(evaluatedObject))
        }
      }
    }
  }
}