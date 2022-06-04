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
          tcType <- TypeChecker.typeCheck(typeExpression, UType(TypeT), env, FullFunction)
          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- Evaluator.asType(nTypeObj, env)
          tc <- TypeChecker.typeCheck(objExpression, UType(nType), env, FullFunction)
          evaluatedObject <- Evaluator(tc.nExpression, env)
          constantObject = Evaluator.stripVersioningU(evaluatedObject, env)
          nObject <- TypeChecker.tagAndNormalizeObject(constantObject, UType(nType), env)
        } yield {
          val command = FullEnvironmentCommand(id.s, nObject)
          Response(Vector(command), command.toString)
        }
      }
      case NewVersionedStatementParse(id, typeExpression) => {
        for {
          tcType <- typeCheck(typeExpression, UType(TypeT), env, FullFunction)
          nTypeObj <- Evaluator(tcType.nExpression, env)
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
          tcType <- typeCheck(typeExpression, UType(TypeT), env, FullFunction)
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
          mapValues <- TypeChecker.typeCheckMap(values, UType(IdentifierT), UType(TypeT), BasicMap, env, FullFunction)
          paramList <- convertMapValuesToParamList(mapValues, env)
        } yield {
          val paramType = TaggedObject(
            UParametrizedCaseT(
              paramList,
              CaseT(Vector.empty, IdentifierT)
            ),
            MapT(
              Environment.toTypeTransform(
                StructT(mapValues, IdentifierT), // TODO: if mapValues has length 1 - should we simplify to the single value?
                TypeT
              ),
              MapConfig(RequireCompleteness, SimpleFunction)
            )
          )

          val command = NewParamTypeCommand(id.s, paramType)
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
              inputT <- currentState match {
                case TaggedObject(upct@UParametrizedCaseT(_, _), _) => CommandMaps.expandParametrizedCaseTInput(upct, env)
                case _ => CommandMaps.getCommandInputOfCommandType(nType, env)
              }

              newEnv = currentState match {
                case TaggedObject(UParametrizedCaseT(parameters, _), _) => {
                  // Eventually - maybe some of these are type classes, or possible expressions? hmm
                  env.newParams(parameters)
                }
                case _ => env
              }

              commandExp <- typeCheck(command, UType(inputT), newEnv, FullFunction)

              commandObj <- Evaluator(commandExp.nExpression, newEnv)
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

              commandExp <- typeCheck(command, UType(inputT), env, FullFunction)

              commandObj <- Evaluator(commandExp.nExpression, env)
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
    mapValues: Vector[(UntaggedObject, NewMapExpression)],
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