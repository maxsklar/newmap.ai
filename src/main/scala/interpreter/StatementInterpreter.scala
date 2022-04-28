package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}

object StatementInterpreter {
  case class Response(
    commands: Vector[EnvironmentCommand],
    output: String
  )

  /*
   * @param sParse The statement parse
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   */
  def apply(
    sParse: EnvStatementParse,
    env: Environment
  ): Outcome[Response, String] = {
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
          val command = FullEnvironmentCommand(id.s, constantObject)
          Response(Vector(command), command.toString)
        }
      }
      case NewVersionedStatementParse(id, typeExpression) => {
        for {
          tcType <- typeCheck(typeExpression, TypeT, env, FullFunction)
          nType <- Evaluator(tcType, env)

          // TODO: Maybe a special error message if this is not a command type
          // - In fact, we have yet to build an actual command type checker
          initValue <- CommandMaps.getDefaultValueOfCommandType(nType, env)
        } yield {
          val command = NewVersionedStatementCommand(id.s, nType)
          Response(Vector(command), command.toString)
        }
      }
      case ForkedVersionedStatementParse(id, forkId) => {
        for {
          vObject <- Evaluator.lookupVersionedObject(forkId.s, env)
        } yield {
          val command = ForkEnvironmentCommand(id.s, vObject)
          Response(Vector(command), command.toString)
        }
      }
      case ApplyCommandStatementParse(id, command) => {
        for {
          versionedObjectLink <- Evaluator.lookupVersionedObject(id.s, env)
          nType = RetrieveType.fromNewMapObject(versionedObjectLink, env)

          inputT <- CommandMaps.getCommandInputOfCommandType(nType, env)
          commandExp <- typeCheck(command, inputT, env, FullFunction)

          commandObj <- Evaluator(commandExp, env)
        } yield {
          val command = ApplyIndividualCommand(id.s, commandObj)
          Response(Vector(command), command.toString)
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
          val command = FullEnvironmentCommand(id.s, evaluatedObject)
          Response(Vector(command), command.toString)
        }
      }
      case ExpressionOnlyStatementParse(exp) => {
        for {
          tc <- TypeChecker.typeCheck(exp, AnyT, env, FullFunction)
          evaluatedObject <- Evaluator(tc, env)
          constantObject = Evaluator.stripVersioning(evaluatedObject, env)
        } yield {
          val command = ExpOnlyEnvironmentCommand(constantObject)
          Response(Vector(command), command.toString)
        }
      }
    }
  }
}