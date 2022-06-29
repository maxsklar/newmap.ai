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
   * @param sParse The statement parses
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   */
  def apply(
    sParse: EnvStatementParse,
    env: Environment
  ): Outcome[Response, String] = {
    sParse match {
      case FullStatementParse(_, id, typeExpression, objExpression) => {
        for {
          tcType <- TypeChecker.typeCheck(typeExpression, TypeT, env, FullFunction)
          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- Evaluator.asType(nTypeObj, env)
          tc <- TypeChecker.typeCheck(objExpression, nType, env, FullFunction)
          evaluatedObject <- Evaluator(tc.nExpression, env)
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
          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- Evaluator.asType(nTypeObj, env)

          // TODO: Maybe a special error message if this is not a command type
          // - In fact, we have yet to build an actual command type checker
          initValue <- CommandMaps.getDefaultValueOfCommandType(nType, env)
        } yield {
          val command = NewVersionedStatementCommand(id.s, nType)
          Response(Vector(command), command.toString)
        }
      }
      case NewTypeStatementParse(id, typeExpression) => {
        for {
          tcType <- typeCheck(typeExpression, TypeT, env, FullFunction)
          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- Evaluator.asType(nTypeObj, env)
        } yield {
          val command = NewTypeCommand(id.s, nType)
          Response(Vector(command), command.toString)
        }
      }
      case NewParamTypeStatementParse(id, params) => {
        val values = params match {
          case CommandList(vs) => vs
          case _ => Vector(params) 
        }

        for {
          mapValues <- TypeChecker.typeCheckMap(values, IdentifierT, TypeT, BasicMap, env, FullFunction)
          paramList <- convertMapValuesToParamList(mapValues, env)
        } yield {
          val command = NewParamTypeCommand(id.s, paramList, CaseT(Vector.empty, IdentifierT))
          Response(Vector(command), command.toString)
        }
      }
      case IterateIntoStatementParse(id, destination) => {
        for {
          iterableObject <- env.lookup(id.s) match {
            case Some(EnvironmentBinding(nObject)) => Success(nObject)
            case _ => Failure(s"Could not lookup $id in environment")
          }

          vDestination <- Evaluator.lookupVersionedObject(destination.s, env)
        } yield {
          println(s"Iterated $iterableObject")

          val command = IterateIntoCommand(iterableObject, vDestination)
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
        Evaluator.lookupVersionedObject(id.s, env) match {
          case Success(versionedObjectLink) => {
            // Now we also need to look this up in the type system!!!
            val nType = RetrieveType.fromNewMapObject(versionedObjectLink, env)
            // TODO - this roundabout way of doing things suggests a refactor
            val currentState = Evaluator.stripVersioning(versionedObjectLink, env)
            
            for {
              inputT <- CommandMaps.getCommandInputOfCommandType(nType, env)

              commandExp <- typeCheck(command, inputT, env, FullFunction)

              commandObj <- Evaluator(commandExp.nExpression, env)
            } yield {
              val command = ApplyIndividualCommand(id.s, commandObj)
              Response(Vector(command), command.toString)
            }
          }
          case Failure(objectLookupFailureMessage) => {
            val typeSystem = env.typeSystem
            val currentState = typeSystem.currentState

            for {
              latestNamespace <- Outcome(typeSystem.historicalMapping.get(currentState), s"Type System missing latest namespace $currentState")
              typeId <- Outcome(latestNamespace.get(id.s), s"Couldn't update variable ${id.s}. Not found in object or type namespace. Object space failure: $objectLookupFailureMessage")

              currentUnderlyingType <- Outcome(typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find underlying type for ${id.s}")

              currentParameterPattern = currentUnderlyingType._1
              currentUnderlyingExp = currentUnderlyingType._2

              underlyingT <- typeSystem.convertToNewMapType(currentUnderlyingExp)

              inputT <- CommandMaps.getTypeExpansionCommandInput(underlyingT, typeSystem)

              newParameterMap <- RetrieveType.getParameterValues(id.s, env)

              newEnv = env.newParams(newParameterMap.toVector)

              commandExp <- typeCheck(command, inputT, newEnv, FullFunction)

              commandObj <- Evaluator(commandExp.nExpression, newEnv)
            } yield {
              val command = ApplyIndividualCommand(id.s, commandObj)
              Response(Vector(command), command.toString)
            }
          }
        }
      }
      case ApplyCommandsStatementParse(id, commands) => {
        throw new Exception("Apply multiple commands not yet implemented")
      }
      case InferredTypeStatementParse(_, id, objExpression) => {
        for {
          // TODO - we need a type inference here!!
          tc <- TypeChecker.typeCheckUnknownType(objExpression, env)
          evaluatedObject <- Evaluator(tc.nExpression, env)
          nObject <- TypeChecker.tagAndNormalizeObject(evaluatedObject, tc.refinedTypeClass, env)
        } yield {
          val command = FullEnvironmentCommand(id.s, nObject)
          Response(Vector(command), command.toString)
        }
      }
      case ExpressionOnlyStatementParse(exp) => {
        for {
          // TODO - we need a type inference here!!
          tc <- TypeChecker.typeCheckUnknownType(exp, env)
          evaluatedObject <- Evaluator(tc.nExpression, env)
          constantObject = Evaluator.stripVersioningU(evaluatedObject, env)
          nObject <- TypeChecker.tagAndNormalizeObject(constantObject, tc.refinedTypeClass, env)
        } yield {
          val command = ExpOnlyEnvironmentCommand(nObject)
          Response(Vector(command), command.toString)
        }
      }
    }
  }

  def convertMapValuesToParamList(
    mapValues: Vector[(UntaggedObject, UntaggedObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapType)], String] = {
    mapValues match {
      case (pattern, expression) +: restOfMapValues => {
        for {
          k <- pattern match {
            case UIdentifier(s) => Success(s)
            case _ => Failure(s"Pattern $pattern should have been an identifier")
          }

          uObject <- Evaluator(expression, env)
          v <- Evaluator.asType(uObject, env)

          restOfResult <- convertMapValuesToParamList(restOfMapValues, env)
        } yield (k -> v) +: restOfResult
      }
      case _ => Success(Vector.empty)
    }
  }
}