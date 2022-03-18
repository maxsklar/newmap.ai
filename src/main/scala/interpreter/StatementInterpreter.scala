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
          nType <- TypeChecker.typeSpecificTypeChecker(typeExpression, env)
          tc <- TypeChecker.typeCheck(objExpression, nType, env)
          evaluatedObject <- Evaluator(tc, env)
        } yield {
          Vector(FullEnvironmentCommand(id.s, evaluatedObject))
        }
      }
      case NewVersionedStatementParse(id, typeExpression) => {
        for {
          nType <- TypeChecker.typeSpecificTypeChecker(typeExpression, env)

          // TODO: Maybe a special error message if this is not a command type
          // - In fact, we have yet to build an actual command type checker
          initValue <- Evaluator.getDefaultValueOfCommandType(nType, env)
        } yield {
          Vector(FullEnvironmentCommand(id.s, VersionedObject(initValue, nType, 0)))
        }
      }
      case ForkedVersionedStatementParse(id, forkExpression) => {
        for {
          tc <- TypeChecker.typeCheck(forkExpression, NewMapO.versionedT, env)

          evaluatedObject <- Evaluator(tc, env, keepVersioning = true)
        } yield {
          Vector(FullEnvironmentCommand(id.s, tc))
        }
      }
      case ApplyCommandStatementParse(id, command) => {
        for {
          commandObj <- TypeChecker.applyCommandTypeChecker(id.s, command, env)
        } yield {
          Vector(ApplyIndividualCommand(id.s, commandObj))
        }
      }
      case ApplyCommandsStatementParse(id, commands) => {
        throw new Exception("Apply multiple commands not yet implemented")
      }
      case InferredTypeStatementParse(_, id, objExpression) => {
        for {
          tc <- TypeChecker.typeCheck(objExpression, AnyT, env)
          evaluatedObject <- Evaluator(tc, env)
        } yield {
          Vector(FullEnvironmentCommand(id.s, evaluatedObject))
        }
      }
      case ExpressionOnlyStatementParse(exp) => {
        for {
          tc <- TypeChecker.typeCheck(exp, AnyT, env)
          evaluatedObject <- Evaluator(tc, env)
        } yield {
          Vector(ExpOnlyEnvironmentCommand(evaluatedObject))
        }
      }
    }
  }
}