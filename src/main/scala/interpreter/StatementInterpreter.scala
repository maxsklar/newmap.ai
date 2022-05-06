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
          nTypeObj <- Evaluator(tcType, env)

          nType <- Evaluator.asType(nTypeObj, env)
          tc <- TypeChecker.typeCheck(objExpression, nType, env, FullFunction)
          evaluatedObject <- Evaluator(tc, env)
          constantObject = Evaluator.stripVersioningU(evaluatedObject, env)

          nObject <- TypeChecker.tagAndNormalizeObject(constantObject, nType, env)
        } yield {
          val command = FullEnvironmentCommand(id.s, nObject)
          Response(Vector(command), command.toString)
        }
      }
      case NewVersionedStatementParse(id, typeExpression) => {
        for {
          tcType <- typeCheck(typeExpression, TypeT, env, FullFunction)
          nTypeObj <- Evaluator(tcType, env)
          nType <- Evaluator.asType(nTypeObj, env)

          // TODO: Maybe a special error message if this is not a command type
          // - In fact, we have yet to build an actual command type checker
          initValue <- CommandMaps.getDefaultValueOfCommandType(nTypeObj, env)
        } yield {
          val command = NewVersionedStatementCommand(id.s, nType)
          Response(Vector(command), command.toString)
        }
      }
      case NewTypeStatementParse(id, typeExpression) => {
        for {
          tcType <- typeCheck(typeExpression, TypeT, env, FullFunction)
          nTypeObj <- Evaluator(tcType, env)
          nType <- Evaluator.asType(nTypeObj, env)
        } yield {
          val command = NewTypeCommand(id.s, nType)
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

          inputT <- if (nType == TypeT) {
            // TODO - this roundabout way of doing things suggests a refactor
            val nObjectStripped = Evaluator.stripVersioning(versionedObjectLink, env)
            for {  
              currentUntagged <- Evaluator.removeTypeTag(nObjectStripped)
              currentAsType <- Evaluator.asType(currentUntagged, env)
              customT = CustomT(versionedObjectLink.key.uuid, currentAsType)
              result <- CommandMaps.getTypeExpansionCommandInput(customT)
            } yield result
          } else {
            CommandMaps.getCommandInputOfCommandType(nType, env)
          }

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
          // TODO - we need a type inference here!!
          tc <- TypeChecker.typeCheckUnknownType(objExpression, env)
          evaluatedObject <- Evaluator(tc._1, env)
          nObject <- TypeChecker.tagAndNormalizeObject(evaluatedObject, tc._2, env)
        } yield {
          val command = FullEnvironmentCommand(id.s, nObject)
          Response(Vector(command), command.toString)
        }
      }
      case ExpressionOnlyStatementParse(exp) => {
        for {
          // TODO - we need a type inference here!!
          tc <- TypeChecker.typeCheckUnknownType(exp, env)
          evaluatedObject <- Evaluator(tc._1, env)
          constantObject = Evaluator.stripVersioningU(evaluatedObject, env)
          nObject <- TypeChecker.tagAndNormalizeObject(constantObject, tc._2, env)
        } yield {
          val command = ExpOnlyEnvironmentCommand(nObject)
          Response(Vector(command), command.toString)
        }
      }
    }
  }
}