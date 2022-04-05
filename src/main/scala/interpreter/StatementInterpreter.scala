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
          tcType <- typeCheck(typeExpression, TypeT, env, FullFunction)
          //_ = println(s"tc from TypeChecker: $tcType")
          nType <- Evaluator(tcType, env)
          //_ = println(s"Type from TypeChecker: $nType")
          tc <- TypeChecker.typeCheck(objExpression, nType, env, FullFunction)
          //_ = println(s"Expression from TypeChecker: $tc")
          evaluatedObject <- Evaluator(tc, env)
          //_ = println(s"evaluatedObject: $evaluatedObject")
          constantObject = Evaluator.stripVersioning(evaluatedObject, env)
        } yield {
          Vector(FullEnvironmentCommand(id.s, constantObject))
        }
      }
      case NewVersionedStatementParse(id, typeExpression) => {
        for {
          tcType <- typeCheck(typeExpression, TypeT, env, FullFunction)
          nType <- Evaluator(tcType, env)

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
          versionedObjectLink <- Evaluator.lookupVersionedObject(id.s, env)
          nType = RetrieveType.fromNewMapObject(versionedObjectLink, env)

          inputT <- Evaluator.getCommandInputOfPureCommandType(nType)
          commandExp <- typeCheck(command, inputT, env, FullFunction)

          commandObj <- Evaluator(commandExp, env)
        } yield {
          Vector(ApplyIndividualCommand(id.s, commandObj))
        }
      }
      case ApplyCommandsStatementParse(id, commands) => {
        throw new Exception("Apply multiple commands not yet implemented")
      }
      case InferredTypeStatementParse(_, id, objExpression) => {
        for {
          tc <- TypeChecker.typeCheck(objExpression, AnyT, env, FullFunction)
          evaluatedObject <- Evaluator(tc, env)
        } yield {
          Vector(FullEnvironmentCommand(id.s, evaluatedObject))
        }
      }
      case ExpressionOnlyStatementParse(exp) => {
        for {
          tc <- TypeChecker.typeCheck(exp, AnyT, env, FullFunction)
          evaluatedObject <- Evaluator(tc, env)
          constantObject = Evaluator.stripVersioning(evaluatedObject, env)
        } yield {
          Vector(ExpOnlyEnvironmentCommand(constantObject))
        }
      }
    }
  }
}