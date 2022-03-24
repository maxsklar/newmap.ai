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
          Vector(NewVersionedStatementCommand(id.s, nType))
        }
      }
      case ForkedVersionedStatementParse(id, forkId) => {
        for {
          vObject <- Evaluator.lookupVersionedObject(forkId.s, env)
        } yield {
          Vector(ForkEnvironmentCommand(id.s, vObject))
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