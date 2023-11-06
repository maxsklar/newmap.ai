package ai.newmap.interpreter

import ai.newmap.interpreter.TypeChecker.{typeCheck, typeCheckMap}
import ai.newmap.model._
import ai.newmap.util.{Failure, Outcome, Success}

object StatementInterpreter {
  case class ReturnValue(
    command: EnvironmentCommand,
    tcParameters: Map[String, NewMapType]
  )

  /*
   * @param sParse The statement parses
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   * @return Do not execute the statement, only reason about the values that it will return to add to the parameters.
   */
  def apply(
    sParse: EnvStatementParse,
    env: Environment,
    tcParameters: Map[String, NewMapType]
  ): Outcome[ReturnValue, String] = {
    sParse match {
      case FullStatementParse(prefix, id, typeExpression, objExpression) => {
        for {
          tcType <- TypeChecker.typeCheck(typeExpression, TypeT, env, FullFunction, tcParameters)
          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- nTypeObj.asType
          
          tc <- TypeChecker.typeCheck(objExpression, nType, env, FullFunction, tcParameters)
          nObject = NewMapObject(tc.nExpression, nType)
        } yield {
          ReturnValue(
            FullEnvironmentCommand(id.s, nObject),
            tcParameters
          )
        }
      }
      case NewVersionedStatementParse(id, typeExpression) => {
        for {
          tcType <- typeCheck(typeExpression, TypeT, env, FullFunction, tcParameters)

          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- nTypeObj.asType

          // TODO: Maybe a special error message if this is not a command type
          // - In fact, we have yet to build an actual command type checker
          _ <- UpdateCommandCalculator.getDefaultValueOfCommandType(nType, env)
        } yield {
          ReturnValue(
            NewVersionedStatementCommand(id.s, nType),
            tcParameters + (id.s -> nType)
          )
        }
      }
      case NewTypeStatementParse(id, typeExpression) => {
        for {
          tcType <- typeCheck(typeExpression, TypeT, env, FullFunction, tcParameters)
          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- nTypeObj.asType
        } yield {
          ReturnValue(
            NewTypeCommand(id.s, nType),
            tcParameters + (id.s -> TypeT)
          )
        }
      }
      case NewParamTypeStatementParse(id, params) => {
        val values = params match {
          case LiteralListParse(vs, MapType) => vs
          case _ => Vector(params) 
        }

        val typeTransform = TypeTransform(IdentifierT, TypeT)

        for {
          mapValues <- typeCheckMap(values, typeTransform, false, env, BasicMap, Map.empty)
          paramList <- convertMapValuesToParamList(mapValues, env)
        } yield {
          ReturnValue(
            NewParamTypeCommand(id.s, paramList, CaseT(UMap(Vector.empty), IdentifierT)),
            tcParameters
          )
        }
      }
      case NewTypeClassStatementParse(id, types) => {
        for {
          typeSetResult <- TypeChecker.typeCheck(
            types,
            MapT(TypeTransform(TypeT, IndexT(UIndex(2))), MapConfig(CommandOutput, PatternMap)),
            env,
            SimpleFunction,
            tcParameters
          )
        } yield {
          ReturnValue(
            NewTypeClassCommand(id.s, typeSetResult.nExpression),
            tcParameters
          )
        }
      }
      case UpdateTypeclassWithTypeCommandParse(id, nTypeParse, implementations) => {
        for {
          nTypeResult <- TypeChecker.typeCheck(nTypeParse, TypeT, env, FullFunction, tcParameters, patternMatchingAllowed = true)
          nTypeObj <- Evaluator(nTypeResult.nExpression, env)
          nType <- nTypeObj.asType

          // What is the type for the implementations?
          // 1) get the implementations required
          typeClassFields: Map[String, TypeClassFieldInfo] = {
            env.typeclassToFieldMapping.get(id.s).getOrElse(Map.empty)
          }

          fieldTypes: Vector[(UntaggedObject, UntaggedObject)] = {
            for {
              (fieldName, fieldInfo) <- typeClassFields.toVector
            } yield {
              val fieldType = MakeSubstitution(
                fieldInfo.nType.asUntagged,
                Map(fieldInfo.wildcardParam -> nTypeObj)
              )

              (UIdentifier(fieldName) -> fieldType)
            }
          }

          // 2) put that into a struct
          implementationRequirements: NewMapType = StructT(UMap(fieldTypes), IdentifierT)

          implementationsResult <- TypeChecker.typeCheck(
            implementations,
            implementationRequirements,
            env,
            FullFunction,
            tcParameters
          )

          mapBindings <- implementationsResult.nExpression.getMapBindings()
        } yield {
          val implementationsTransformed = mapBindings.flatMap(binding => {
            for {
              identifier <- binding._1 match {
                case UIdentifier(id) => Some(id)
                case _ => None
              }
            } yield identifier -> binding._2
          })

          ReturnValue(
            UpdateTypeclassWithTypeCommand(id.s, nType, implementationsTransformed),
            tcParameters
          )
        }
      }
      case UpdateTypeclassWithFieldCommandParse(id, typeParameter, fieldType, fieldName, implementations, isCommand) => {
        //println("In here: " + sParse)
        for {
          fieldTypeResult <- TypeChecker.typeCheck(fieldType, TypeT, env, FullFunction, tcParameters + (typeParameter -> TypeT))
          //_ = println(s"fieldTypeResult: $fieldTypeResult")
          fieldTypeObject <- Evaluator(fieldTypeResult.nExpression, env)
          //_ = println(s"fieldTypeObject: $fieldTypeObject")
          fieldT <- fieldTypeObject.asType

          //_ = println(s"fieldT: $fieldT")

          implementationsResult <- TypeChecker.typeCheck(
            implementations,
            MapT(TypeTransform(TypeT, fieldT), MapConfig(PartialMap, BasicMap)),
            env,
            FullFunction,
            tcParameters
          )

          //_ = println(s"implementationsResult: $implementationsResult")

          mapBindings <- implementationsResult.nExpression.getMapBindings()

          //_ = println(s"mapBindings: $mapBindings")
        } yield {
          val implementationsTransformed: Vector[(NewMapType, UntaggedObject)] = mapBindings.flatMap(binding => {
            for {
              implementationTypeResult <- TypeChecker.typeCheck(fieldType, TypeT, env, FullFunction, tcParameters).toOption
              implT <- implementationTypeResult.nExpression.asType.toOption
            } yield implT -> binding._2
          })

          //println(s"implementationsTransformed: $implementationsTransformed")

          ReturnValue(
            UpdateTypeclassWithFieldCommand(id, typeParameter, fieldT, fieldName, implementationsTransformed, isCommand),
            tcParameters
          )
        }
      }
      case IterateIntoStatementParse(iterableExp, destination) => {
        for {
          // TODO - we need a type inference here!!
          tc <- TypeChecker.typeCheckUnknownType(iterableExp, env, Map.empty)
          evaluatedObject <- Evaluator(tc.nExpression, env)
          constantObject = Evaluator.stripVersioningU(evaluatedObject, env)
          iterableObjectCandidate <- TypeChecker.tagAndNormalizeObject(constantObject, tc.refinedTypeClass, env)

          // NOTE: This is a very inefficient way (we are calling the whole function to see if we can call the function)
          // - In the future, this should be taken into account by the type checker
          // - the type checker, instead of using unknown type, will get a hint that this is an iterable object, and
          //   will use that hint to build it!
          iterableObject <- IterationUtils.iterateObject(iterableObjectCandidate, env) match {
            case Success(_) => Success(iterableObjectCandidate)
            case _ => {
              for {
                tcType <- TypeChecker.typeCheck(iterableExp, TypeT, env, FullFunction, tcParameters)
                uType <- Evaluator(tcType.nExpression, env)
                iterableTypeCandidate = NewMapObject(uType, TypeT)
              } yield iterableTypeCandidate
            }
          }

          // destination can either be a CHANNEL or it can be a VERSIONED OBJECT
          // which is it?
          // TODO - eventually merge the 2 concepts!

          channelTypeOpt = env.channelIdToType.get(destination.s)

          command <- channelTypeOpt match {
            case Some(channelType) => {
              for {
                tObject <- TypeConverter.attemptConvertObjectToType(iterableObject, channelType, env)
              } yield {
                IterateIntoChannel(tObject.uObject, UIdentifier(destination.s))
              }
            }
            case None => {
              // IT's an object, not a channel
              for {
                vDestination <- env.lookupVersionedObject(destination.s)
              } yield {
                IterateIntoCommand(iterableObject, destination.s)
              }
            }
          }
        } yield {
          ReturnValue(command, tcParameters)
        }
      }
      case ForkedVersionedStatementParse(id, forkId) => {
        for {
          vObject <- env.lookupVersionedObject(forkId.s)
        } yield {
          ReturnValue(
            ForkEnvironmentCommand(id.s, vObject.key),
            tcParameters + (id.s -> vObject.nType)
          )
        }
      }
      case ApplyCommandStatementParse(id, command) => {
        (env.lookupVersionedObject(id.s), tcParameters.get(id.s)) match {
          case (_, Some(nType)) => {
            for {
              inputT <- UpdateCommandCalculator.getCommandInputOfCommandType(nType, env)
              commandExp <- typeCheck(command, inputT, env, FullFunction, tcParameters)
            } yield {
              ReturnValue(
                ApplyIndividualCommand(id.s, commandExp.nExpression),
                tcParameters
              )
            }
          }
          case (Success(versionedObjectLink), _) => {
            for {
              inputT <- UpdateCommandCalculator.getCommandInputOfCommandType(versionedObjectLink.nType, env)
              commandExp <- typeCheck(command, inputT, env, FullFunction, tcParameters)
            } yield {
              ReturnValue(
                ApplyIndividualCommand(id.s, commandExp.nExpression),
                tcParameters
              )
            }
          }
          case (Failure(objectLookupFailureMessage), _) => {
            for {
              currentUnderlyingType <- env.typeSystem.currentUnderlyingType(id.s)

              inputT <- TypeExpander.getTypeExpansionCommandInput(currentUnderlyingType._2, env.typeSystem)

              newParameterMap <- RetrieveType.getParameterValues(id.s, env)

              commandExp <- typeCheck(command, inputT, env, FullFunction, newParameterMap)
            } yield {
              ReturnValue(
                ApplyIndividualCommand(id.s, commandExp.nExpression),
                tcParameters
              )
            }
          }
        }
      }
      case ApplyCommandsStatementParse(_, _) => {
        throw new Exception("Apply multiple commands not yet implemented")
      }
      case ApplyCustomCommandParse(id, commandName, commandParams) => {
        for {
          // TODO: what if this is a type expansion?
          versionedObjectLink <- env.lookupVersionedObject(id.s)

          currentFields <- Evaluator.applyFunction(
            env.typeToFieldMapping,
            versionedObjectLink.nType.asUntagged,
            env,
            TypeMatcher
          )

          fieldMapping <- Evaluator.applyFunction(
            currentFields,
            UIdentifier(commandName.s),
            env
          )

          fieldMappingIsCommand <- Evaluator.applyFunction(
            fieldMapping,
            UIndex(1),
            env
          )

          _ <- fieldMappingIsCommand match {
            case UIndex(1) => Success(())
            case _ => Failure("Field " + commandName + " is not a command.")
          }

          fieldMappingResult <- Evaluator.applyFunction(
            fieldMapping,
            UIndex(0),
            env
          )

          returnType <- fieldMappingResult match {
            case UCase(t, _) => Success(t)
            case _ => Failure("Unknown fieldMappingResult: " + fieldMappingResult)
          }

          returnT <- returnType.asType

          returnInputT <- Outcome(returnT.inputTypeOpt(None), "Input type not found: " + returnT.displayString(env))

          commandParams <- typeCheck(commandParams, returnInputT, env, FullFunction, tcParameters)
        } yield {
          val command  = ApplyIndividualCommand(id.s, UCase(UIdentifier(commandName.s), commandParams.nExpression))
          ReturnValue(command, tcParameters)
        }
      }
      case AddChannelParse(channelId, channelTypeParse) => {
        val channel = UIdentifier(channelId.s)
        for {
          tcType <- TypeChecker.typeCheck(channelTypeParse, TypeT, env, FullFunction, tcParameters)
          nTypeObj <- Evaluator(tcType.nExpression, env)
          nType <- nTypeObj.asType
        } yield {
          ReturnValue(
            AddChannel(channel, nType),
            tcParameters
          )
        }
      }
      case ConnectChannelParse(channelId, obj) => {
        val channel = UIdentifier(channelId.s)
        env.lookupVersionedObject(obj.s) match {
          case Success(versionedObjectLink) => {
            for {
              inputT <- UpdateCommandCalculator.getCommandInputOfCommandType(versionedObjectLink.nType, env)

              channelType = env.channelIdToType.get(channelId.s).getOrElse(UndefinedT)

              // So channelType is what's coming from the channel, and inputT is what's required
              // So we want to make sure that channelType is convertible to inputT

              //_ = println(s"channelType: ${channelType.displayString(env)} -- ${inputT.displayString(env)}")
              
              // Looks like channelType is String
              // And inputT is character
              // Should be doable to connect!

              channelCanBeConnectedToObject <- IterationUtils.isIteratableToType(channelType, inputT, env)
              _ <- Outcome.failWhen(!channelCanBeConnectedToObject, s"can't connect channel ${channelId.s} of type ${channelType.displayString(env)} to object ${obj.s} of type ${inputT.displayString(env)}")
            } yield {
              ReturnValue(
                ConnectChannel(channel, obj.s),
                tcParameters
              )
            }
          }
          case Failure(_) => {
            throw new Exception("Cannot yet connect channel to a type")
          }
        }
      }
      case DisconnectChannelParse(channelId, obj) => {
        val channel = UIdentifier(channelId.s)
        env.lookupVersionedObject(obj.s) match {
          case Success(_) => {
            // No need for type checking when we are disconnecting
            Success(
              ReturnValue(
                DisconnectChannel(channel, obj.s),
                tcParameters
              )
            )
          }
          case Failure(_) => {
            throw new Exception("Cannot yet disconnect channel from a type")
          }
        }
      }
      case WriteToChannelParse(channelId, command) => {
        val channel = UIdentifier(channelId.s)
        val nType = env.channelIdToType.get(channelId.s).getOrElse(UndefinedT)
        for {
          tc <- TypeChecker.typeCheck(command, nType, env, FullFunction, tcParameters)
        } yield {
          ReturnValue(
            OutputToChannel(tc.nExpression, channel),
            tcParameters
          )
        }
      }
      case InferredTypeStatementParse(_, id, objExpression) => {
        for {
          // This infers the type using the output "refinedTypeClass"
          tc <- TypeChecker.typeCheckUnknownType(objExpression, env, Map.empty)
        } yield {
          val nObject = NewMapObject(tc.nExpression, tc.refinedTypeClass)
          ReturnValue(
            FullEnvironmentCommand(id.s, nObject),
            tcParameters
          )
        }
      }
      case ExpressionOnlyStatementParse(exp) => {
        for {
          // This infers the type using the output "refinedTypeClass"
          tc <- TypeChecker.typeCheckUnknownType(exp, env, Map.empty)
        } yield {
          val nObject = NewMapObject(tc.nExpression, tc.refinedTypeClass)

          ReturnValue(
            ExpOnlyEnvironmentCommand(nObject),
            tcParameters
          )
        }
      }
      case NewVersionedFieldParse(featureSet, typeParse, id, returnTypeParse, parseTree) => {
        val typeTransformParse = KeyValueBinding(typeParse, returnTypeParse)

        for {
          // TODO: I'm not sure if "SimpleFunction" is what we want here.
          // - Think about this more, and try to make it more obvious what to use in the future.
          typeTransformTC <- TypeChecker.typeCheck(typeTransformParse, TypeTransformT(true), env, SimpleFunction, Map.empty)

          typeTransformEval <- Evaluator(typeTransformTC.nExpression, env)

          typeTransform <- NewMapType.convertToTypeTransform(typeTransformEval)

          // TODO - what if typeTransform.valueType has a parameter?
          useCommandMap = UpdateCommandCalculator.getDefaultValueOfCommandType(typeTransform.valueType, env).isSuccess
          completeness = if (useCommandMap) CommandOutput else RequireCompleteness
          mapConfig = MapConfig(completeness, featureSet)

          mapType = MapT(typeTransform, mapConfig)

          newEnv = if (featureSet.getLevel >= WellFoundedFunction.getLevel) {
            // If its recursive, add it to the environment
            env.newCommand(
              NewVersionedFieldCommand(id, mapType, ParamId(id), false)
            )
          } else {
            env
          }

          // I still don't know if "FullFunction" is right here
          valueTC <- TypeChecker.typeCheck(parseTree, mapType, newEnv, FullFunction, Map.empty)
        } yield {
          ReturnValue(
            NewVersionedFieldCommand(id, valueTC.refinedTypeClass, valueTC.nExpression, false),
            tcParameters
          )
        }
      }
      case NewCommandParse(
        featureSet: MapFeatureSet,
        typeParse: ParseTree,
        takingTypeParse: ParseTree,
        selfPattern: String,
        commandName: String,
        inputPattern: ParseTree,
        outputRule: ParseTree
      ) => {
        for {
          baseTypePatternTC <- TypeChecker.typeCheck(typeParse, TypeT, env, PatternMap, Map.empty, true)
          takingTypePatternTC <- TypeChecker.typeCheck(takingTypeParse, TypeT, env, PatternMap, baseTypePatternTC.tcParameters)

          baseT <- baseTypePatternTC.nExpression.asType
          takingTypeT <- takingTypePatternTC.nExpression.asType
          innerTypeTransform = TypeTransform(takingTypeT, baseT)

          fullTypeTransform = TypeTransform(
            baseT,
            MapT(
              innerTypeTransform,
              MapConfig(RequireCompleteness, featureSet)
            )
          )

          fullMapType = MapT(fullTypeTransform, MapConfig(RequireCompleteness, featureSet))

          resultTypeCheck <- TypeChecker.typeCheck(
            KeyValueBinding(
              IdentifierParse(selfPattern),
              KeyValueBinding(inputPattern, outputRule)
            ),
            fullMapType,
            env,
            FullFunction, // TODO - is this right?
            Map.empty
          )
        } yield {
          ReturnValue(
            NewVersionedFieldCommand(
              commandName,
              resultTypeCheck.refinedTypeClass,
              resultTypeCheck.nExpression,
              true
            ),
            tcParameters
          )
        }
      }
      case EmptyStatement => Success(ReturnValue(EmptyEnvironmentCommand, tcParameters))
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
          v <- uObject.asType

          restOfResult <- convertMapValuesToParamList(restOfMapValues, env)
        } yield (k -> v) +: restOfResult
      }
      case _ => Success(Vector.empty)
    }
  }
}