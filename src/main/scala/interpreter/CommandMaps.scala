package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Evaluates an expression that's already been type checked
object CommandMaps {
  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)

  // This is neccesary for when we have types as tagged objects and types and standalones.
  // Once types are always tagged objects - we should be able to remove this
  def normalizeTypeTaggedObject(nObject: NewMapObject): NewMapObject = {
    nObject match {
      case TaggedObject(UType(t), TypeT) => t
      case _ => nObject
    }
  }

  /*
   * This is getDefaultValueOfCommandType being slowly written into newmap code
   */
  def getDefaultValueOfCommandTypeFromEnv(nType: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    env.lookup("_default") match {
      case Some(EnvironmentValue(defaultMap, BoundStatus)) => {
        for {
          mapValues <- Evaluator.stripVersioning(defaultMap, env) match {
            case TaggedObject(UMap(values), _) => Success(values)
            case _ => Failure("_default doesn't look the way we expect")
          }
          
          // I wanted to call "applyFunctionAttempt" here, but we can't call getDefaultValueOfCommandType
          // otherwise, we get an infinite loop
          result <- Evaluator.attemptPatternMatchInOrder(mapValues, Evaluator.stripVersioning(nType, env), env) match {
            case Success(s) => Success(s)
            case Failure(f) => Failure(f.toString)
          }
        } yield result
      }
      case _ => {
        Failure("_default doesn't exist yet")
      }
    }
  }

  def getDefaultValueOfCommandType(nType: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    getDefaultValueOfCommandTypeFromEnv(nType, env).rescue(f => {
      getDefaultValueOfCommandTypeHardcoded(nType, env)
    })
  }

  def getDefaultValueOfCommandTypeHardcoded(nType: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    nType match {
      // TODO - start removing these in favor of newmap code!
      case OrBooleanT => Success(TaggedObject(UIndex(0), OrBooleanT))
      case ExpandingSubsetT(_, _) | MapT(_, _, MapConfig(CommandOutput, _, _, _)) => Success(TaggedObject(UMap(Vector.empty), nType))
      case MapT(TaggedObject(UIndex(0), _), _, MapConfig(RequireCompleteness, _, _, _)) => Success(TaggedObject(UMap(Vector.empty), nType))
      case MapT(TaggedObject(UMap(m), _), _, MapConfig(RequireCompleteness, _, _, _)) if (m.isEmpty) => Success(TaggedObject(UMap(Vector.empty), nType))
      case TaggedObject(UIndex(i), _) if i > 0 => Success(TaggedObject(UIndex(0), Index(i)))
      case DataTypeT(typeParams) => {
        Success(TaggedObject(UMap(Vector.empty), DataTypeT(typeParams)))
      }
      case structT@StructT(TaggedObject(UMap(parameterList), MapT(keyType, _, _))) => {
        if (isEmptySet(keyType, env)) {
          // If the key set is empty, no parameters need to be specified, even if a pattern exists!
          // TODO - maybe the code in this case block should be entirely driven by keyType!
          Success(TaggedObject(UMap(Vector.empty), structT))
        } else {   
          for {
            defaultValue <- getDefaultValueFromStructParams(parameterList, env)
          } yield {
            TaggedObject(UMap(defaultValue), structT)
          }
        }
      }
      //case CaseT(cases) => {
        // In order for cases to have a default value, there's have to be 2 things:
        // - casesType must have a default (a default case) - call it casesType.default
        // - casesToType(casesType.default) is a type that must have a default case
        //throw new Exception(s"Case Types do not have a default value -- $nType")
        //Failure("Case Types do not have a default value")
      //}
      case _ => {
        Failure(s"$nType is not a command type, error in type checker")
      }
    }
  }

  def isEmptySet(nType: NewMapObject, env: Environment): Boolean = {
    Evaluator.stripVersioning(nType, env) match {
      case TaggedObject(UMap(umap), _) => umap.isEmpty
      case TaggedObject(UIndex(0), _) => true
      case _ => false
    }
  }

  def getCommandInputOfCommandType(
    nType: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => Success(
        NewMapO.emptyStruct
      )
      case OrBooleanT => Success(Index(2))
      case mapT@MapT(inputType, outputType, MapConfig(CommandOutput, _, _, _)) => {
        // TO incorporate TableT:
        // Look at input type
        // See if input type is itself a command type
        // Then, if it is - include that in the command input!

        for {
          outputCommandT <- getCommandInputOfCommandType(outputType, env)
        } yield {
          StructT(
            TaggedObject(
              UMap(Vector(
                ObjectPattern(UIndex(0)) -> ObjectExpression(inputType),
                ObjectPattern(UIndex(1)) -> ObjectExpression(outputCommandT)
              )),
              MapT(
                Index(2),
                TypeT,
                MapConfig(RequireCompleteness, BasicMap)
              )
            )
          )
        }
      }
      case MapT(keyType, requiredValues, _) => {
        // In this case, there must be a key expansion type
        // TODO: enforce this?

        // Key Expansion + requiredValue expansion
        // What if Key expansion is a case? (for now we don't allow this, only basic map)
        for {
          keyExpansionCommandT <- getCommandInputOfCommandType(RetrieveType.fromNewMapObject(keyType, env), env)
        } yield {
          keyExpansionCommandT match {
            case StructT(TaggedObject(UMap(items), _)) if (items.length == 0) => {
              // TODO - this is an ugly exception.. we need a better way to add fields to a struct
              // (particularly an empty struct like in this case)
              requiredValues
            }
            case _ => {
              StructT(
                TaggedObject(
                  UMap(Vector(
                    ObjectPattern(UIndex(0)) -> ObjectExpression(keyExpansionCommandT),
                    ObjectPattern(UIndex(1)) -> ObjectExpression(requiredValues)
                  )),
                  MapT(Index(2), TypeT, MapConfig(RequireCompleteness, BasicMap))
                )
              )
            }
          }
        }
      }
      case ExpandingSubsetT(parentType, _) => Success(parentType)
      // This should really be a case type instead of a typeT
      case DataTypeT(typeParams) => {
        Success(
          StructT(
            TaggedObject(
              UMap(Vector(
                ObjectPattern(UIndex(0)) -> ObjectExpression(IdentifierT),
                ObjectPattern(UIndex(1)) -> ObjectExpression(TypeT)
              )),
              MapT(Index(2), TypeT, MapConfig(RequireCompleteness, BasicMap))
            )
          )
        )
      }
      case structT@StructT(TaggedObject(UMap(parameterList), MapT(keyType, valueType, config))) => {
        // Checks:
        val wildcardPatternExists = parameterList.map(_._1).exists(k => SubtypeUtils.isCatchallPattern(k, nType, env))

        val keyTypeC = Evaluator.stripVersioning(keyType, env)

        if (wildcardPatternExists) {
          for {
            keyExpansionCommandT <- getCommandInputOfCommandType(RetrieveType.fromNewMapObject(keyType, env), env)
          } yield {
            // We are free to add more items to this struct!
            // There are going to be 2 inputs:
            // - The key expansion command
            // - The value, which now needs to be cased because the type of the value depends on the key

            // TODO - unfortunately, we can't actually write a command for this can we!!
            // More must be done here
            CaseT(TaggedObject(UMap(parameterList), MapT(keyExpansionCommandT, valueType, config)))
          }
        } else {
          Failure("The command type hasn't been implemented yet for this type of struct")
        }
      }
      case structT@StructT(params) => {
        Failure("Structs as commands haven't been implemented yet")
      }
      case CaseT(cases) => {
        Failure("Cases as commands haven't been implemented yet")
      }
      case _ => {
        throw new Exception(s"$nType is not a command type, error in type checker")
        Failure(s"$nType is not a command type, error in type checker")
      }
    }
  }

  case class UpdateVersionedOResponse(
    newState: NewMapObject,
    output: NewMapObject
  )

  // This is a weird artifact needed for updateVersionedO.. it's going to improve
  // By having objects tagged the old way automatically updated to the new way
  def retagObject(nObject: NewMapObject, newTypeTag: NewMapObject): NewMapObject = {
    nObject match {
      case TaggedObject(untagged, nType) => TaggedObject(untagged, newTypeTag)
      case _ => nObject
    }
  }

  def updateVersionedO(
    current: NewMapObject,
    command: NewMapObject,
    env: Environment
  ): Outcome[UpdateVersionedOResponse, String] = {
    RetrieveType.fromNewMapObject(current, env) match {
      case CountT => {
        current match {
          case TaggedObject(UIndex(i), _) => {
            for {
              newState <- Evaluator.applyFunctionAttempt(
                TaggedObject(IncrementFunc, MapT(CountT, CountT, MapConfig(RequireCompleteness, SimpleFunction))),
                current,
                env
              )
            } yield UpdateVersionedOResponse(newState, TaggedObject(UIndex(i), newState))
          }
          case _ => {
            throw new Exception("Invalid count in versioning upgrade")
          }
        }
      }
      case OrBooleanT => {
        (current, command) match {
          case (TaggedObject(UIndex(i), _), TaggedObject(UIndex(j), _)) => {
            // Only tell us if the bit finally "flipped"
            val result = if (i == 1 || j == 1) 1 else 0
            val output = if (i == 0 && j == 1) 1 else 0
            Success(
              UpdateVersionedOResponse(
                TaggedObject(UIndex(result), OrBooleanT),
                TaggedObject(UIndex(output), Index(2))
            ))
          }
        case _ => Failure("This didn't work")
        }
      }
      case mapT@MapT(inputType, outputType, MapConfig(CommandOutput, featureSet, _, _)) => {
        for {
          input <- Evaluator.applyFunctionAttempt(command, Index(0), env)
          commandForInput <- Evaluator.applyFunctionAttempt(command, Index(1), env)

          currentResultForInput <- Evaluator.applyFunctionAttempt(current, input, env)

          newResultForInput <- updateVersionedO(currentResultForInput, commandForInput, env)

          mapValues <- current match {
            case TaggedObject(UMap(values), _) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          untaggedInput <- Evaluator.removeTypeTag(input)

          newMapValues = (ObjectPattern(untaggedInput) -> ObjectExpression(newResultForInput.newState)) +: mapValues.filter(x => x._1 != ObjectPattern(untaggedInput))
        } yield {
          UpdateVersionedOResponse(TaggedObject(UMap(newMapValues), mapT), NewMapO.emptyStruct)
        }
      }
      case mapT@MapT(keyType, requiredValues, MapConfig(style, features, _, _)) => {
        for {
          keyExpansionCommandT <- getCommandInputOfCommandType(
            RetrieveType.fromNewMapObject(keyType, env),
            env
          )

          result <- keyExpansionCommandT match {
            case StructT(TaggedObject(UMap(items), _)) if (items.length == 0) => {
              // TODO - this is an ugle exception.. we need a better way to add fields to a struct
              // (particularly an empty struct like in this case)
              Success((TaggedObject(UMap(Vector.empty), keyExpansionCommandT), command))
            }
            case _ => {
              for {
                keyField <- Evaluator.applyFunctionAttempt(command, Index(0), env)
                valueField <- Evaluator.applyFunctionAttempt(command, Index(1), env)
              } yield (keyField, valueField)
            }
          }

          (keyExpansionCommand, valueExpansionCommand) = result

          updateKeyTypeResponse <- updateVersionedO(keyType, keyExpansionCommand, env)

          newTableType = MapT(updateKeyTypeResponse.newState, requiredValues, MapConfig(style, features))
                  
          mapValues <- current match {
            case TaggedObject(UMap(values), _) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          updateKeyUntagged <- Evaluator.removeTypeTag(updateKeyTypeResponse.output)

          newMapping = ObjectPattern(updateKeyUntagged) -> ObjectExpression(valueExpansionCommand)

          prepNewValues = for {
            value <- mapValues

            // Remove old value
            if (value._1 != ObjectPattern(updateKeyUntagged))
          } yield (value._1 -> value._2)

          newMapValues = newMapping +: prepNewValues
        } yield UpdateVersionedOResponse(TaggedObject(UMap(newMapValues), newTableType), NewMapO.emptyStruct)
      }
      case ExpandingSubsetT(parentType, allowPatterns) => {
        //current is going be of type SubsetT(parentT)
        // and the isMember function is going to have a value of isSubsetOr
        Evaluator.stripVersioning(current, env) match {
          case TaggedObject(UMap(values), _) => {
            // BUILD command
            val isMemberCommand = TaggedObject(
              UMap(Vector(
                ObjectPattern(UIndex(0)) -> ObjectExpression(command),
                ObjectPattern(UIndex(1)) -> ObjectExpression(TaggedObject(UIndex(1), Index(2)))
              )),
              StructT(
                TaggedObject(
                  UMap(Vector(
                    ObjectPattern(UIndex(0)) -> ObjectExpression(parentType),
                    ObjectPattern(UIndex(1)) -> ObjectExpression(Index(2))
                  )),
                  MapT(Index(2), TypeT, MapConfig(RequireCompleteness, BasicMap))
                )
              )
            )

            val isMember = TaggedObject(UMap(values), MapT(parentType, OrBooleanT, MapConfig(CommandOutput, BasicMap)))

            for {
              result <- updateVersionedO(isMember, isMemberCommand, env)
            } yield {
              // TODO - eventually the output will be different (set of new items??)
              UpdateVersionedOResponse(
                retagObject(result.newState, ExpandingSubsetT(parentType, allowPatterns)),
                command
              )
            }
          }
          case _ => Failure(s"Didn't get what I expected for ExpandingSubsetT $parentType -- $current -- $command")
        }
      }
      case DataTypeT(typeParams) => {
        current match {
          case TaggedObject(uCases@UMap(values), _) => {
            val uConstructors = values.map(x => x._1 -> ObjectExpression(TaggedObject(UIndex(1), OrBooleanT)))
            val expandingUConstructors = TaggedObject(UMap(uConstructors), ExpandingSubsetT(IdentifierT, false))
            val caseMap = TaggedObject(UMap(values), MapT(expandingUConstructors, TypeT, MapConfig(RequireCompleteness, BasicMap)))

            for{
              result <- updateVersionedO(caseMap, command, env)
            } yield {
              val retagged = retagObject(result.newState, DataTypeT(typeParams))

              UpdateVersionedOResponse(
                retagged,
                result.output
              )
            }
          }
          case _ => {
            Failure(s"Cannot expand type $current")
          }
        }
      }
      case StructT(params) => {
        command match {
          case TaggedObject(UCase(constructor, input), _) => {
            for {
              mapValues <- current match {
                case TaggedObject(UMap(values), _) => Success(values)
                case _ => Failure(s"Couldn't get map values from $current")
              }

              caseConstructorType = RetrieveType.retrieveInputTypeFromFunctionObj(params, env)
              taggedConstructor = normalizeTypeTaggedObject(TaggedObject(constructor, RetrieveType.getParentType(caseConstructorType, env)))

              typeForInput <- Evaluator.applyFunctionAttempt(params, taggedConstructor, env)
              taggedInput = normalizeTypeTaggedObject(TaggedObject(input, typeForInput))

              newParams <- params match {
                // TODO - this part is currently only written for _default
                case TaggedObject(UMap(values), MapT(keyType, valueType, config)) => {
                  for {
                    result <- updateVersionedO(keyType, taggedConstructor, env)
                  } yield {
                    val newKeyType = result.newState
                    TaggedObject(UMap(values), MapT(newKeyType, valueType, config))
                  }
                }
                case _ => {
                  Failure(s"Unexpected struct params: $params")
                }
              }

            } yield {
              val newMapValues = (ObjectPattern(constructor) -> ObjectExpression(taggedInput)) +: mapValues.filter(x => x._1 != ObjectPattern(constructor))
              UpdateVersionedOResponse(TaggedObject(UMap(newMapValues), StructT(newParams)), NewMapO.emptyStruct)
            }
          }
          case _ => {
            Failure(s"A) Structs as commands haven't been implemented yet -- $params -- $command")
          }
        }
      }
      case CaseT(cases) => {
        Failure("Cases as commands haven't been implemented yet")
      }
      case _ => {
        Failure(s"$current is not a command type, error in type checker")
      }
    }
  }

  case class UpdateVersionedObjectResponse(
    newVersion: Long,
    uuid: UUID,
    newValue: NewMapObject,
    output: NewMapObject
  )

  def updateVersionedObject(
    id: String,
    command: NewMapObject,
    env: Environment
  ): Outcome[UpdateVersionedObjectResponse, String] = {
    for {
      versionLink <- Evaluator.lookupVersionedObject(id, env)
      latestVersion <- Evaluator.latestVersion(versionLink.key.uuid, env)
      currentState <- Evaluator.currentState(versionLink.key.uuid, env)
      result <- updateVersionedO(currentState, command, env)
    } yield {
      UpdateVersionedObjectResponse(latestVersion + 1, versionLink.key.uuid, result.newState, result.output)
    }
  }

  def getDefaultValueFromStructParams(
    params: Vector[(NewMapPattern, NewMapExpression)],
    env: Environment
  ): Outcome[Vector[(NewMapPattern, NewMapExpression)], String] = {
    params match {
      case (id, obj) +: restOfParams => {
        for {
          restOfParamsDefault <- getDefaultValueFromStructParams(restOfParams, env)
          paramDefault <- getDefaultValueOfCommandType(RetrieveType(obj, env), env)
        } yield {
          (id -> ObjectExpression(paramDefault)) +: restOfParamsDefault
        }
      }
      case _ => Success(Vector.empty)
    }
  }
}