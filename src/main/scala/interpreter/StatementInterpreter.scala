package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}

object StatementInterpreter {
  /*
   * @param sParse The statement parses
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   */
  def apply(
    sParse: EnvStatementParse,
    env: Environment
  ): Outcome[EnvironmentCommand, String] = {
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
          FullEnvironmentCommand(id.s, nObject)
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
          NewVersionedStatementCommand(id.s, nType)
        }
      }
      case NewTypeStatementParse(id, typeExpression) => {
        for {
          tcType <- typeCheck(typeExpression, TypeT, env, FullFunction)
          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- Evaluator.asType(nTypeObj, env)
        } yield {
          NewTypeCommand(id.s, nType)
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
          NewParamTypeCommand(id.s, paramList, CaseT(Vector.empty, IdentifierT))
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
          IterateIntoCommand(iterableObject, destination.s)
        }
      }
      case ForkedVersionedStatementParse(id, forkId) => {
        for {
          vObject <- Evaluator.lookupVersionedObject(forkId.s, env)
        } yield {
          ForkEnvironmentCommand(id.s, vObject)
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
              ApplyIndividualCommand(id.s, commandObj)
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
              ApplyIndividualCommand(id.s, commandObj)
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
          FullEnvironmentCommand(id.s, nObject)
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
          ExpOnlyEnvironmentCommand(nObject)
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