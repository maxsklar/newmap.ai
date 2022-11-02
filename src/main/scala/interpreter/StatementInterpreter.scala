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

          // Problem is WITH_TYPE!!

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
      case NewTypeClassStatementParse(id, typeTransformParse) => {
        val typeOfTypeTransform = MapT(
          UMap(Vector(env.typeSystem.typeToUntaggedObject(TypeT) -> env.typeSystem.typeToUntaggedObject(TypeT))),
          MapConfig(
            CommandOutput,
            SimpleFunction
          )
        )

        for {
          typeTransformResult <- TypeChecker.typeCheck(typeTransformParse, typeOfTypeTransform, env, FullFunction)

          typeTransform <- typeTransformResult.nExpression match {
            case UMap(values) => Success(values)
            case _ => Failure("Invalue type transform: ${typeTransformResult.nExpression}")
          }
        } yield {
          NewTypeClassCommand(id.s, typeTransform)
        }
      }
      case IterateIntoStatementParse(id, destination) => {
        for {
          iterableObject <- env.lookup(id.s) match {
            case Some(EnvironmentBinding(nObject)) => Success(nObject)
            case _ => Failure(s"Could not lookup $id in environment")
          }

          // destination can either be a CHANNEL or it can be a VERSIONED OBJECT
          // which is it?
          // TODO - eventually merge the 2 concepts!

          val channelTypeOpt = env.channelIdToType.get(destination.s)

          command <- channelTypeOpt match {
            case Some(channelType) => {
              for {
                uObject <- SubtypeUtils.attemptConvertObjectToType(iterableObject, channelType, env)
              } yield {
                IterateIntoChannel(uObject, UIdentifier(destination.s))
              }
            }
            case None => {
              // IT's an object, not a channel
              for {
                vDestination <- Evaluator.lookupVersionedObject(destination.s, env)
              } yield {
                IterateIntoCommand(iterableObject, destination.s)
              }
            }
          }
        } yield command
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
      case AddChannelParse(channelId, channelTypeParse) => {
        val channel = UIdentifier(channelId.s)
        for {
          tcType <- TypeChecker.typeCheck(channelTypeParse, TypeT, env, FullFunction)
          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- Evaluator.asType(nTypeObj, env)
        } yield {
          AddChannel(channel, nType)
        }
      }
      case ConnectChannelParse(channelId, obj) => {
        val channel = UIdentifier(channelId.s)
        Evaluator.lookupVersionedObject(obj.s, env) match {
          case Success(versionedObjectLink) => {
            val nType = RetrieveType.fromNewMapObject(versionedObjectLink, env)

            for {
              inputT <- CommandMaps.getCommandInputOfCommandType(nType, env)

              channelType = env.channelIdToType.get(channelId.s).getOrElse(UndefinedT)

              // So channelType is what's coming from the channel, and inputT is what's required
              // So we want to make sure that channelType is convertible to inputT

              //_ = println(s"channelType: ${channelType.displayString(env)} -- ${inputT.displayString(env)}")
              
              // Looks like channelType is String
              // And inputT is character
              // Should be doable to connect!

              channelCanBeConnectedToObject <- IterationUtils.isIteratableToType(channelType, inputT, env)
              _ <- Outcome.failWhen(!channelCanBeConnectedToObject, s"can't connect channel ${channelId.s} of type ${channelType.displayString(env)} to object ${obj.s} of type ${inputT.displayString(env)}")
            } yield ConnectChannel(channel, obj.s)
          }
          case Failure(reason) => {
            throw new Exception("Cannot yet connect channel to a type")
          }
        }
      }
      case DisconnectChannelParse(channelId, obj) => {
        val channel = UIdentifier(channelId.s)
        Evaluator.lookupVersionedObject(obj.s, env) match {
          case Success(versionedObjectLink) => {
            // No need for type checking when we are disconnecting
            Success(DisconnectChannel(channel, obj.s))
          }
          case Failure(reason) => {
            throw new Exception("Cannot yet disconnect channel from a type")
          }
        }
      }
      case WriteToChannelParse(channelId, command) => {
        val channel = UIdentifier(channelId.s)
        val nType = env.channelIdToType.get(channelId.s).getOrElse(UndefinedT)
        for {
          tc <- TypeChecker.typeCheck(command, nType, env, FullFunction)
        } yield {
          OutputToChannel(tc.nExpression, channel)
        }
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