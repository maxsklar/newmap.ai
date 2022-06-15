package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Evaluates an expression that's already been type checked
object CommandMaps {
  def IndexTN(i: Long): NewMapType = IndexT(UIndex(i))

  /*
   * This is getDefaultValueOfCommandType being slowly written into newmap code
   */
  def getDefaultValueOfCommandTypeFromEnv(nType: UntaggedObject, env: Environment): Outcome[UntaggedObject, String] = {
    env.lookup("_default") match {
      case Some(EnvironmentBinding(defaultMap)) => {
        for {
          mapValues <- Evaluator.stripVersioning(defaultMap, env) match {
            case TaggedObject(UMap(values), _) => Success(values)
            case _ => Failure("_default doesn't look the way we expect")
          }
          
          // I wanted to call "applyFunctionAttempt" here, but we can't call getDefaultValueOfCommandType
          // otherwise, we get an infinite loop
          result <- Evaluator.attemptPatternMatchInOrder(mapValues, nType, env) match {
            case Success(s) => Success(s)
            case Failure(f) => Failure(f.toString)
          }

          uObject <- Evaluator(result, env)
        } yield uObject
      }
      case _ => {
        Failure("_default doesn't exist yet")
      }
    }
  }

  def getDefaultValueOfCommandType(nType: NewMapType, env: Environment): Outcome[UntaggedObject, String] = {
    getDefaultValueOfCommandTypeFromEnv(env.typeSystem.typeToUntaggedObject(nType), env).rescue(f => {
      getDefaultValueOfCommandTypeHardcoded(nType, env)
    })
  }

  val defaultUMap = UMap(Vector.empty)

  def getDefaultValueOfCommandTypeHardcoded(nType: NewMapType, env: Environment): Outcome[UntaggedObject, String] = {
    nType match {
      // TODO - start removing these in favor of newmap code!
      case IndexT(UIndex(i)) if i > 0 => Success(UIndex(0)) //REmove?
      case TypeT => Success(UCase(UIdentifier("UndefinedType"), UStruct(Vector.empty)))
      //case MapT(_, _, MapConfig(CommandOutput, _, _)) => Success(defaultUMap)
      case MapT(_, MapConfig(CommandOutput, _, _)) => Success(defaultUMap)
      case MapT(UMap(typeTransform), MapConfig(RequireCompleteness, _, _)) => {
        if (typeTransformHasEmptyKey(typeTransform, env)) {
          Success(defaultUMap)
        } else {
          throw new Exception(s"Can't start off map with key in typeTransform $typeTransform")
          Failure(s"Can't start off map with key in typeTransform $typeTransform")
        }
      }
      case StructT(params, _, CommandOutput, _) => {
        Success(defaultUMap)
      }
      case TypeClassT(typeTransform, typesInTypeClass) if (typesInTypeClass.isEmpty) => {
        Success(defaultUMap)
      }
      case _ => Failure(s"Type $nType has no default value")
    }
  }

  def typeTransformHasEmptyKey(
    typeTransform: Vector[(UntaggedObject, UntaggedObject)],
    env: Environment
  ): Boolean = {
    val patterns = typeTransform.map(_._1)

    if (patterns.length == 0) {
      true
    } else if (patterns.length == 1) {
      env.typeSystem.convertToNewMapType(patterns.head) match {
        case Failure(_) => false
        case Success(nType) => nType match {
          case IndexT(UIndex(0)) | IndexT(UInit) => true
          case SubtypeT(UMap(m), _, _) => m.isEmpty
          case _ => false // TODO - unimplemented
        }
      }
    } else {
      false
    }
  }

  // Shouldn't this be called expand type?
  def getTypeExpansionCommandInput(
    nType: NewMapType,
    typeSystem: NewMapTypeSystem
  ): Outcome[NewMapType, String] = {
    nType match {
      case IndexT(_) => Success(NewMapO.emptyStruct) // Where to insert the new value?
      case CaseT(cases, parentType, featureSet) => {
        Success(StructT(
          Vector(
            UIndex(0) -> typeSystem.typeToUntaggedObject(parentType),
            UIndex(1) -> typeSystem.typeToUntaggedObject(HistoricalTypeT(typeSystem.currentState))
          ),
          IndexTN(2)
        ))
      }
      case StructT(cases, parentType, _, featureSet) => {
        Success(StructT(
          Vector(
            UIndex(0) -> typeSystem.typeToUntaggedObject(parentType),
            UIndex(1) -> typeSystem.typeToUntaggedObject(SubtypeT(IsCommandFunc, HistoricalTypeT(typeSystem.currentState)))
          ),
          IndexTN(2)
        ))
      }
      case SubtypeT(isMember, parentType, featureSet) => Success(parentType)
      //case MapT(keyType, valueType, config) => getTypeExpansionCommandInput(valueType, typeSystem)
      case CustomT(name, UStruct(params)) => {
        val currentState = typeSystem.currentState

        for {
          currentMapping <- Outcome(typeSystem.historicalMapping.get(currentState), s"Current type mapping $currentState not found")
          currentTypeId <- Outcome(currentMapping.get(name), s"$name must be defined")
          currentUnderlyingType <- Outcome(typeSystem.typeToUnderlyingType.get(currentTypeId), s"Couldn't find underlying type for $name")

          currentParameterPattern = currentUnderlyingType._1
          currentUnderlyingExp = currentUnderlyingType._2

          underlyingT <- typeSystem.convertToNewMapType(currentUnderlyingExp)

          commandInput <- getTypeExpansionCommandInput(underlyingT, typeSystem)
        } yield commandInput
      }
      case _ => Failure(s"Unable to expand key: $nType")
    }
  }

  val untaggedIdentity: UntaggedObject = UMap(Vector(UWildcardPattern("_") -> ParamId("_")))

  case class ExpandKeyResponse(
    newType: NewMapType,
    newValueOpt: Option[UntaggedObject],
    converter: UntaggedObject // This is a function that can convert from the old type to the new type
  )

  def expandType(
    nType: NewMapType,
    command: UntaggedObject,
    env: Environment
  ): Outcome[ExpandKeyResponse, String] = {
    nType match {
      case IndexT(UIndex(i)) => {
        val newType = IndexTN(i + 1)
        Success(ExpandKeyResponse(newType, Some(UIndex(i)), untaggedIdentity))
      }
      case CaseT(cases, parentType, featureSet) => {
        val uConstructors = cases.map(x => x._1 -> UIndex(1))
        val constructorsSubtype = SubtypeT(UMap(uConstructors), parentType, featureSet)
        val caseMap = TaggedObject(UMap(cases), MapT(
          env.toTypeTransform(constructorsSubtype, TypeT), 
          MapConfig(RequireCompleteness, BasicMap)
        ))

        for {
          newCaseMap <- updateVersionedObject(caseMap, command, env)
          untaggedNewCaseMap <- Evaluator.removeTypeTag(newCaseMap)

          newCaseName <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)
        } yield {
          untaggedNewCaseMap match {
            case UMap(newCases) => {
              ExpandKeyResponse(
                CaseT(newCases, parentType, featureSet),
                Some(UCase(newCaseName, UWildcardPattern("_"))),
                untaggedIdentity
              )
            }
            case _ => {
              throw new Exception("This shouldn't happen!!")
            }
          }
        }
      }
      case SubtypeT(isMember, parentType, featureSet) => {
        val isMemberMap = TaggedObject(isMember, MapT(
          env.toTypeTransform(parentType, BooleanT),
          MapConfig(CommandOutput, BasicMap)
        ))

        val adjustedCommand = UMap(Vector(
          UIndex(0) -> command,
          UIndex(1) -> UIndex(1)
        ))

        for {
          newMembersMap <- updateVersionedObject(isMemberMap, adjustedCommand, env)
          untaggedNewMembersMap <- Evaluator.removeTypeTag(newMembersMap)
        } yield {
          ExpandKeyResponse(
            SubtypeT(untaggedNewMembersMap, parentType, featureSet),
            Some(command),
            untaggedIdentity
          )
        }
      }
      case MapT(UMap(typeTransform), config) => {
        if (typeTransform.length != 1) {
          throw new Exception(s"Can't yet handle typeTransforms without a single binding: $typeTransform")
        }

        typeTransform.head match {
          case (keyType, valueType) => {
            for {
              keyT <- env.typeSystem.convertToNewMapType(keyType)
              valueT <- env.typeSystem.convertToNewMapType(valueType)
              expandedValueInfo <- expandType(valueT, command, env)
            } yield {
              val newType = MapT(env.toTypeTransform(keyT, expandedValueInfo.newType), config)

              // TODO - the valueTypes need to be converted - figure out a way to do this!!
              // - Sort of an implementation of mapValues
              //expandedValeInfo.converter
              
              ExpandKeyResponse(nType, None, untaggedIdentity)
            }
          }
          case _ => {
            Failure(s"Cannot yet handle commands for generic map $typeTransform")
          }
        }
      }
      case CustomT(name, params) => {
        // This only occurs if we have a custom type within a custom type - so this won't be called for a while.
        // strategy: get underlying type from the type system, turn it into a NewMapType, and then call this on it!
        val typeSystem = env.typeSystem
        val currentState = typeSystem.currentState
        for {
          currentMapping <- Outcome(typeSystem.historicalMapping.get(currentState), s"Current type mapping $currentState not found")
          currentTypeId <- Outcome(currentMapping.get(name), s"$name must be defined")
          currentUnderlyingTypeInfo <- Outcome(typeSystem.typeToUnderlyingType.get(currentTypeId), s"Couldn't find underlying type for $name")

          currentParameterPattern = currentUnderlyingTypeInfo._1
          currentUnderlyingType = currentUnderlyingTypeInfo._2
          currentUnderlyingT <- env.typeSystem.convertToNewMapType(currentUnderlyingType)
          response <- expandType(currentUnderlyingT, command, env)
        } yield response
      }
      case _ => Failure(s"Unable to expand key: $nType -- with command $command")
    }
  }

  def getCommandInputOfCommandType(
    nType: NewMapType,
    env: Environment
  ): Outcome[NewMapType, String] = {
    nType match {
      case CountT => Success(
        NewMapO.emptyStruct
      )
      case BooleanT => Success(IndexTN(2))
      case MapT(UMap(typeTransform), MapConfig(CommandOutput, _, _)) => {
        // Now instead of giving the structT, we must give something else!!
        // we have typeTransform
        // we need a pair (a, b) that satisfies the type transform
        // This is a (pattern/expression) pairing
        // the type of the expression depends on the value of the pattern??
        // -- therefore, this is a binding!
        // -- should we make a binding a first class object, and should typeTransform just be a binding?
        
        if (typeTransform.length != 1) {
          throw new Exception(s"Can't yet handle typeTransforms without a single binding: $typeTransform")
        }

        typeTransform.head match {
          case (keyType, valuesType) => {
            for {
              valuesT <- Evaluator.asType(valuesType, env)
              outputCommandT <- getCommandInputOfCommandType(valuesT, env)
            } yield {
              StructT(
                Vector(
                  UIndex(0) -> keyType,
                  UIndex(1) -> env.typeSystem.typeToUntaggedObject(outputCommandT)
                ),
                IndexTN(2)
              )
            }
          }
          case _ => {
            Failure(s"Cannot yet handle commands for generic map $typeTransform")
          }
        }
      }
      case MapT(UMap(typeTransform), _) => {
        if (typeTransform.length != 1) {
          throw new Exception(s"Can't yet handle typeTransforms without a single binding: $typeTransform")
        }

        typeTransform.head match {
          case (keyType, requiredValuesType) => {
            // In this case, there must be a key expansion type
            // TODO: enforce this?

            // Key Expansion + requiredValue expansion
            // What if Key expansion is a case? (for now we don't allow this, only basic map)
            for {
              keyT <- env.typeSystem.convertToNewMapType(keyType)
              requiredValuesT <- env.typeSystem.convertToNewMapType(requiredValuesType)
              keyExpansionCommandT <- getTypeExpansionCommandInput(keyT, env.typeSystem)
            } yield {
              keyExpansionCommandT match {
                case StructT(items, _, _, _) if (items.length == 0) => {
                  // TODO - this is an ugly exception.. we need a better way to add fields to a struct
                  // (particularly an empty struct like in this case)
                  requiredValuesT
                }
                case _ => {
                  StructT(
                    Vector(
                      UIndex(0) -> env.typeSystem.typeToUntaggedObject(keyExpansionCommandT),
                      UIndex(1) -> requiredValuesType
                    ),
                    IndexTN(2)
                  )
                }
              }
            }
          }
          case _ => {
            Failure(s"Cannot yet handle commands for generic map $typeTransform")
          }
        }
      }
      case structT@StructT(parameterList, parentFieldType, RequireCompleteness, featureSet) => {
        // Change to CaseT because we are adding a single parameter!
        // Are we allowed to change an old parameter? Let's say sure.
        Success(CaseT(parameterList, parentFieldType, featureSet))
      }
      case structT@StructT(parameterList, parentFieldType, CommandOutput, featureSet) => {
        // Change to CaseT because we are adding a single parameter!
        // Are we allowed to change an old parameter? Let's say sure.
        Success(CaseT(parameterList, parentFieldType, featureSet))
      }
      case CaseT(cases, _, _) => {
        Failure("Cases as commands haven't been implemented yet")
      }
      case TypeClassT(typeTransform, typesInTypeClass) => {
        Success(CaseT(typeTransform, TypeT, SimpleFunction))
      }
      case _ => {
        Failure(s"B) $nType is not a command type, error in type checker")
      }
    }
  }

  def updateVersionedObject(
    current: NewMapObject,
    command: UntaggedObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    RetrieveType.fromNewMapObject(current, env) match {
      case CountT => {
        current match {
          case TaggedObject(count, nType) => {
            for {
              c <- count match {
                case UIndex(i) => Success(i)
                case UInit => Success(0L)
                case _ => Failure(s"Couldn't interpret count value: $count")
              }

              newState = UCase(UIdentifier("Inc"), UIndex(c))
              result <- TypeChecker.tagAndNormalizeObject(newState, nType, env)
            } yield result
          }
          case _ => {
            throw new Exception("Invalid count in versioning upgrade")
          }
        }
      }
      case BooleanT => {
        for {
          currentValue <- current match {
            case TaggedObject(UIndex(i), _) => Success(i)
            case TaggedObject(UInit, _) => Success(0)
            case _ => Failure(s"Couldn't interpret current value: $current")
          }

          j <- command match {
            case UIndex(i) => Success(i)
            case UInit => Success(0)
            case _ => Failure(s"Couldn't interpret command $command")
          }
        } yield {
          val result = if (currentValue == 1 || j == 1) 1 else 0
          TaggedObject(UIndex(result), BooleanT)
        }
      }
      case mapT@MapT(UMap(typeTransform), MapConfig(CommandOutput, featureSet, _)) => {
        if (typeTransform.length != 1) {
          throw new Exception(s"Not implemented: type transform must have exactly 1 mapping: $typeTransform")
        }

        for {
          outputType <- typeTransform.head match {
            case (inputPattern, outputT) => {
              for {
                ouT <- Evaluator.asType(outputT, env)
              } yield ouT
            }
            case _ => {
              Failure(s"Not implement for generic type transform: $typeTransform")
            }
          }

          input <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)
          commandForInput <- Evaluator.applyFunctionAttempt(command, UIndex(1), env)

          untaggedCurrent <- Evaluator.removeTypeTag(current)

          currentResultForInput <- Evaluator.applyFunctionAttempt(untaggedCurrent, input, env)

          newResultForInput <- updateVersionedObject(
            TaggedObject(currentResultForInput, outputType),
            commandForInput,
            env
          )

          mapValues <- untaggedCurrent match {
            case UMap(values) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          untaggedNewState <- Evaluator.removeTypeTag(newResultForInput)

          newMapValues = (input -> untaggedNewState) +: mapValues.filter(x => x._1 != input)
        } yield {
          TaggedObject(UMap(newMapValues), mapT)
        }
      }
      case MapT(UMap(typeTransform), MapConfig(style, features, _)) => {
        if (typeTransform.length != 1) {
          throw new Exception(s"Can't yet handle typeTransforms without a single binding: $typeTransform")
        }

        typeTransform.head match {
          case (keyType, requiredValuesType) => {
            for {
              keyT <- env.typeSystem.convertToNewMapType(keyType)
              requiredValuesT <- env.typeSystem.convertToNewMapType(requiredValuesType)

              keyExpansionCommandT <- getTypeExpansionCommandInput(keyT, env.typeSystem)

              result <- keyExpansionCommandT match {
                case StructT(items, _, _, _) if (items.length == 0) => {
                  // TODO - this is an ugle exception.. we need a better way to add fields to a struct
                  // (particularly an empty struct like in this case)
                  Success((TaggedObject(UMap(Vector.empty), keyExpansionCommandT), command))
                }
                case _ => {

                  for {
                    commandPatterns <- command match {
                      case UMap(patterns) => Success(patterns)
                      case _ => Failure(s"Unexpected command shape $command")
                    }

                    keyField <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)

                    valueExpression <- Evaluator.attemptPatternMatchInOrder(commandPatterns, UIndex(1), env)
                  } yield {
                    (TaggedObject(keyField, keyT), valueExpression)
                  }
                }
              }

              (keyExpansionCommand, valueExpansionExpression) = result

              updateKeyUntagged <- Evaluator.removeTypeTag(keyExpansionCommand)
              
              // Really we're updating the key??
              updateKeyTypeResponse <- expandType(keyT, updateKeyUntagged, env)
              // TODO- we need to do something with updateKeyTypeResponse.converter


              newTableType = MapT(
                env.toTypeTransform(updateKeyTypeResponse.newType, requiredValuesT),
                MapConfig(style, features)
              )
                      
              mapValues <- current match {
                case TaggedObject(UMap(values), _) => Success(values)
                case _ => Failure(s"Couldn't get map values from $current")
              }

              newPattern <- Outcome(updateKeyTypeResponse.newValueOpt, "Cannot expand type: $keyT and get a new pattern to match")

              prepNewValues = for {
                value <- mapValues

                // Remove old value
                if (value._1 != updateKeyUntagged)
              } yield (value._1 -> value._2)

              newMapValues = (newPattern -> valueExpansionExpression) +: prepNewValues
            } yield {
              TaggedObject(UMap(newMapValues), newTableType)
            }
          }
          case _ => {
            Failure(s"Cannot yet handle commands for generic map $typeTransform")
          }
        }
      }
      case StructT(params, parentFieldType, _, _) => {
        command match {
          case UCase(constructor, input) => {
            for {
              mapValues <- current match {
                case TaggedObject(UMap(values), _) => Success(values)
                case _ => Failure(s"Couldn't get map values from $current")
              }

              // TODO - this part is currently only written for _default
              // Params need to be updated!!
              newParams = params
            } yield {
              val newMapValues = (constructor -> input) +: mapValues.filter(x => x._1 != constructor)
              TaggedObject(UMap(newMapValues), StructT(newParams, parentFieldType))
            }
          }
          case _ => {
            Failure(s"A) Structs as commands haven't been implemented yet -- $params -- $command")
          }
        }
      }
      case TypeClassT(typeTransform, typesInTypeClass) => {
        command match {
          case UCase(constructor, input) => {
            for {
              mapValues <- current match {
                case TaggedObject(UMap(values), _) => Success(values)
                case _ => Failure(s"Couldn't get map values from $current")
              }
            } yield {
              val newTypesInClass = constructor +: typesInTypeClass.filter(x => x != constructor)
              val newMapValues = (constructor -> input) +: mapValues.filter(x => x._1 != constructor)
              TaggedObject(UMap(newMapValues), TypeClassT(typeTransform, newTypesInClass))
            }
          }
          case _ => {
            Failure(s"Wrong input for typeClassT -- $current -- $command")
          }
        }
      }
      case _ => {
        Failure(s"C) $current is not a command type, error in type checker")
      }
    }
  }

  def getDefaultValueFromStructParams(
    params: Vector[(UntaggedObject, UntaggedObject)],
    env: Environment
  ): Outcome[Vector[(UntaggedObject, UntaggedObject)], String] = {
    params match {
      case (fieldName, typeOfFieldExp) +: restOfParams => {
        for {
          typeOfField <- Evaluator(typeOfFieldExp, env)
          typeOfFieldT<- Evaluator.asType(typeOfField, env)
          paramDefault <- getDefaultValueOfCommandType(typeOfFieldT, env)
          restOfParamsDefault <- getDefaultValueFromStructParams(restOfParams, env)
        } yield {
          (fieldName -> paramDefault) +: restOfParamsDefault
        }
      }
      case _ => Success(Vector.empty)
    }
  }
}