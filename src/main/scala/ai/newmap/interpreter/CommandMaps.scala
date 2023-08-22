package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Evaluates an expression that's already been type checked
object CommandMaps {
  def IndexTN(i: Long): NewMapType = IndexT(UIndex(i))

  def getDefaultValueOfCommandType(nType: NewMapType, env: Environment): Outcome[UntaggedObject, String] = {
    getDefaultValueOfCommandTypeFromEnv(env.typeSystem.typeToUntaggedObject(nType), env).rescue(f => {
      getDefaultValueOfCommandTypeHardcoded(nType, env)
    })
  }

  val defaultUMap = UMap(Vector.empty)

  /*
   * This is getDefaultValueOfCommandType being slowly written into newmap code
   * It is a map that takes certain types (called command types) and outputs their default, or initial value
   * The initial value of Count is 0, for example
   */
  def getDefaultValueOfCommandTypeFromEnv(nType: UntaggedObject, env: Environment): Outcome[UntaggedObject, String] = {
    for {
      defaultTypeId <- Outcome(env.typeSystem.currentMapping.get("_default"), "Couldn't find _default typeClass")

      defaultUnderlyingType <- Outcome(env.typeSystem.typeToUnderlyingType.get(defaultTypeId), "Couldn't find _default underlying typeclass")

      defaultParameterPattern = defaultUnderlyingType._1
      defaultUnderlyingExp = defaultUnderlyingType._2

      defaultUnderlyingExpT <- env.typeSystem.convertToNewMapType(defaultUnderlyingExp)

      mapValues <- defaultUnderlyingExpT match {
        case TypeClassT(_, values) => Success(values)
        case _ => Failure(s"Not a type class: ${defaultUnderlyingExpT.displayString(env)}")
      }

      // I wanted to call "applyFunctionAttempt" here, but we can't call getDefaultValueOfCommandType
      // otherwise, we get an infinite loop
      result <- Evaluator.attemptPatternMatchInOrder(mapValues, nType, env, TypeMatcher) match {
        case Success(s) => Success(s)
        case Failure(f) => Failure(f.toString)
      }

      uObject <- Evaluator(result, env)
    } yield uObject
  }

  def getDefaultValueOfCommandTypeHardcoded(nType: NewMapType, env: Environment): Outcome[UntaggedObject, String] = {
    nType match {
      // TODO - start removing these in favor of newmap code!
      case IndexT(UIndex(i)) if i > 0 => Success(UIndex(0)) //REmove?
      //case MapT(_, _, MapConfig(CommandOutput, _, _)) => Success(defaultUMap)
      case MapT(_, MapConfig(CommandOutput, _, _, _, _)) => Success(defaultUMap)
      case MapT(typeTransform, MapConfig(RequireCompleteness, _, _, _, _)) => {
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
      case StructT(params, _, _, _) if (params.length == 0) => {
        Success(defaultUMap)
      }
      case FunctionalSystemT(functionTypes) if functionTypes.length == 0 => {
        Success(defaultUMap)
      }
      /*case TypeClassT(typeTransform, implementation) if (implementation.isEmpty) => {
        Success(defaultUMap)
      }*/
      case CharacterT => Success(UCharacter('\u0000'))
      case CustomT("Array", nType) => Success(UCase(UIndex(0), UStruct(Vector.empty)))
      case CustomT("String", _) => Success(UCase(UIndex(0), UStruct(Vector.empty))) // Replace this line with a conversion!
      case WithStateT(uuid, underlying) => {
        getDefaultValueOfCommandTypeHardcoded(underlying, env)
      }
      case _ => Failure(s"Type ${nType.displayString(env)} has no default value")
    }
  }

  def typeTransformHasEmptyKey(
    typeTransform: UntaggedObject,
    env: Environment
  ): Boolean = {
    TypeChecker.typeTransformInputTypes(typeTransform) match {
      case Success(patterns) => {
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
      case Failure(_) => false
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
        

      case TypeClassT(typeTransform, implementation) => {
        for {
          bindings <- getTypeTransformBindings(typeTransform)
        } yield {
          //CaseT(Vector(typeTransform.key -> typeTransform.value), TypeT, SimpleFunction)
          
          // Its not requireCompleteness because we're not giving the full map here.
          // - This is inelegant, because it shouldn't be CommandOutput either.
          // - TODO: somehow indicate when we don't want to check for completeness
          // -- Or maybe our typetransform should indicate a subset??
          //MapT(typeTransform, MapConfig(CommandOutput, BasicMap))
          
          //MapT(UMap(typeTransform), MapConfig(RequireCompleteness, SimpleFunction))
          StructT(bindings, TypeT, CommandOutput, BasicMap)
        }
      }
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
    newValues: Seq[UntaggedObject],
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
        Success(ExpandKeyResponse(newType, Vector(UIndex(i)), untaggedIdentity))
      }
      case CaseT(cases, parentType, featureSet) => {
        val uConstructors = cases.map(x => x._1 -> UIndex(1))
        val constructorsSubtype = SubtypeT(UMap(uConstructors), parentType, featureSet)
        val mapConfig = MapConfig(RequireCompleteness, BasicMap)

        val caseMap = NewMapObject(UMap(cases), MapT(
          env.toTypeTransform(constructorsSubtype, TypeT), 
          mapConfig
        ))

        for {
          newCaseMap <- updateVersionedObject(caseMap, command, env)

          newCaseName <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)
        } yield {
          newCaseMap.uObject match {
            case UMap(newCases) => {
              ExpandKeyResponse(
                CaseT(newCases, parentType, featureSet),
                Vector(UCase(newCaseName, UWildcardPattern("_"))),
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
        val isMemberMap = NewMapObject(isMember, MapT(
          env.toTypeTransform(parentType, BooleanT),
          MapConfig(CommandOutput, BasicMap)
        ))

        val adjustedCommand = UMap(Vector(
          UIndex(0) -> command,
          UIndex(1) -> UIndex(1)
        ))

        for {
          newMembersMap <- updateVersionedObject(isMemberMap, adjustedCommand, env)
        } yield {
          ExpandKeyResponse(
            SubtypeT(newMembersMap.uObject, parentType, featureSet),
            Vector(command),
            untaggedIdentity
          )
        }
      }
      case MapT(typeTransform, config) => {
        for {
          typeTransformKeyValue <- checkTypeTransformForSingleBinding(typeTransform)

          keyType = typeTransformKeyValue._1
          valueType = typeTransformKeyValue._2

          keyT <- env.typeSystem.convertToNewMapType(keyType)
          valueT <- env.typeSystem.convertToNewMapType(valueType)
          expandedValueInfo <- expandType(valueT, command, env)
        } yield {
          val newType = MapT(env.toTypeTransform(keyT, expandedValueInfo.newType), config)

          // TODO - the valueTypes need to be converted - figure out a way to do this!!
          // - Sort of an implementation of mapValues
          //expandedValeInfo.converter
          
          ExpandKeyResponse(nType, Nil, untaggedIdentity)
        }
      }
      case TypeClassT(typeTransform, implementation) => {
        command match {
          case UMap(mappings) => {
            val keys = mappings.map(_._1)
            val newImplementation = mappings ++ implementation.filter(x => !keys.contains(x._1))

            Success(ExpandKeyResponse(
              TypeClassT(typeTransform, newImplementation),
              keys,
              untaggedIdentity
            ))
          }
          case _ => {
            Failure(s"Wrong input for typeClassT -- ${nType.displayString(env)} -- $command")
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
    env: Environment,
    typeSystemIdOpt: Option[UUID] = None
  ): Outcome[NewMapType, String] = {
    nType match {
      case CountT => Success(
        NewMapO.emptyStruct
      )
      case BooleanT => Success(IndexTN(2))
      case MapT(typeTransform, MapConfig(CommandOutput, _, _, _, _)) => {
        // Now instead of giving the structT, we must give something else!!
        // we have typeTransform
        // we need a pair (a, b) that satisfies the type transform
        // This is a (pattern/expression) pairing
        // the type of the expression depends on the value of the pattern??
        // -- therefore, this is a binding!
        // -- should we make a binding a first class object, and should typeTransform just be a binding?
        
        for {
          typeTransformKeyValue <- checkTypeTransformForSingleBinding(typeTransform)
          keyType = typeTransformKeyValue._1
          valuesType = typeTransformKeyValue._2

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
      case MapT(typeTransform, _) => {
        // In this case, there must be a key expansion type
        // TODO: enforce this?

        // Key Expansion + requiredValue expansion
        // What if Key expansion is a case? (for now we don't allow this, only basic map)

        for {
          typeTransformKeyValue <- checkTypeTransformForSingleBinding(typeTransform)
          keyType = typeTransformKeyValue._1
          requiredValuesType = typeTransformKeyValue._2

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
      case FunctionalSystemT(functionTypes) => {
        Success(StructT(
          Vector(
            UIndex(0) -> env.typeSystem.typeToUntaggedObject(IdentifierT),
            UIndex(1) -> env.typeSystem.typeToUntaggedObject(NewMapO.taggedObjectT)
          ),
          IndexTN(2)
        ))
      }
      case structT@StructT(parameterList, parentFieldType, RequireCompleteness, featureSet) => {
        // We may have the option to wanting to expand this struct!
        // TODO: This is one of the cases where the type CHANGES when you update the object
        // - should this be allowed? This may be a problem.

        // Expand the number of fields in this struct like so!
        val fieldExpansionCommandT = parentFieldType
        // We are freely adding to an object and changing the type of it's fields
        // This means that we need to give
        // A) A field expansion command
        // B) The tagged object that goes in there (so both type and object)
        Success(StructT(
          Vector(
            UIndex(0) -> env.typeSystem.typeToUntaggedObject(fieldExpansionCommandT),
            UIndex(1) -> env.typeSystem.typeToUntaggedObject(NewMapO.taggedObjectT)
          ),
          IndexTN(2)
        ))
      }
      case structT@StructT(parameterList, parentFieldType, CommandOutput, featureSet) => {
        // Change to CaseT because we are adding a single parameter!
        // Are we allowed to change an old parameter? Let's say sure.
        Success(CaseT(parameterList, parentFieldType, featureSet))
      }
      // This should be custom defined
      case CustomT("Array", nType) => env.typeSystem.convertToNewMapType(nType)
      case CustomT(typeName, params) => {
        val typeSystemId = typeSystemIdOpt.getOrElse(env.typeSystem.currentState)
        val typeSystemMapping = env.typeSystem.historicalMapping.get(typeSystemId).getOrElse(Map.empty) 

        for {
          typeId <- Outcome(typeSystemMapping.get(typeName), s"Couldn't find type: $typeName")
          underlyingTypeInfo <- Outcome(env.typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find type: $typeName -- $typeId")

          (underlyingPattern, underlyingExp) = underlyingTypeInfo

          patternMatchSubstitutions <- Evaluator.attemptPatternMatch(underlyingPattern, params, StandardMatcher, env)

          underlyingType = MakeSubstitution(underlyingExp, patternMatchSubstitutions)

          underlyingT <- env.typeSystem.convertToNewMapType(underlyingType)
          result <- getCommandInputOfCommandType(underlyingT, env)
        } yield {
          result
        }
      }
      case WithStateT(typeSystemId, nType) => {
        getCommandInputOfCommandType(nType, env, Some(typeSystemId))
      }
      /*case CaseT(cases, _, _) => {
        Success(UndefinedT)
      }*/
      case _ => {
        println(s"Got undefined for ${nType.displayString(env)}")
        Success(UndefinedT)
      }
    }
  }

  def updateVersionedObject(
    current: NewMapObject,
    command: UntaggedObject,
    env: Environment,
    typeSystemIdOpt: Option[UUID] = None
  ): Outcome[NewMapObject, String] = {
    current.nType match {
      case nType@CountT => {
        for {
          c <- current.uObject match {
            case UIndex(i) => Success(i)
            case UInit => Success(0L)
            case _ => Failure(s"Couldn't interpret count value: ${current.uObject}")
          }

          newState = UCase(UIdentifier("Inc"), UIndex(c))
          result <- TypeChecker.tagAndNormalizeObject(newState, nType, env)
        } yield result
      }
      case BooleanT => {
        for {
          currentValue <- current.uObject match {
            case UIndex(i) => Success(i)
            case UInit => Success(0)
            case _ => Failure(s"Couldn't interpret current value: $current")
          }

          j <- command match {
            case UIndex(i) => Success(i)
            case UInit => Success(0)
            case _ => Failure(s"Couldn't interpret command $command")
          }
        } yield {
          val result = if (currentValue == 1 || j == 1) 1 else 0
          NewMapObject(UIndex(result), BooleanT)
        }
      }
      case mapT@MapT(typeTransform, MapConfig(CommandOutput, featureSet, _, _, _)) => {
        for {
          typeTransformKeyValue <- checkTypeTransformForSingleBinding(typeTransform)

          inputPattern = typeTransformKeyValue._1
          outputT = typeTransformKeyValue._2

          outputType <- Evaluator.asType(outputT, env)

          input <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)
          commandForInput <- Evaluator.applyFunctionAttempt(command, UIndex(1), env)

          currentResultForInput <- Evaluator.applyFunctionAttempt(current.uObject, input, env)

          newResultForInput <- updateVersionedObject(
            NewMapObject(currentResultForInput, outputType),
            commandForInput,
            env
          )

          mapValues <- current.uObject match {
            case UMap(values) => Success(values)
            case _ => Failure(s"Couldn't get map values from ${current.uObject}")
          }

          newMapValues = (input -> newResultForInput.uObject) +: mapValues.filter(x => x._1 != input)
        } yield {
          NewMapObject(UMap(newMapValues), mapT)
        }
      }
      case MapT(typeTransform, MapConfig(style, features, _, _, _)) => {
        for {
          typeTransformKeyValue <- checkTypeTransformForSingleBinding(typeTransform)
          keyType = typeTransformKeyValue._1
          requiredValuesType = typeTransformKeyValue._2

          keyT <- env.typeSystem.convertToNewMapType(keyType)
          requiredValuesT <- env.typeSystem.convertToNewMapType(requiredValuesType)

          keyExpansionCommandT <- getTypeExpansionCommandInput(keyT, env.typeSystem)

          result <- keyExpansionCommandT match {
            case StructT(items, _, _, _) if (items.length == 0) => {
              // TODO - this is an ugle exception.. we need a better way to add fields to a struct
              // (particularly an empty struct like in this case)
              Success((NewMapObject(UMap(Vector.empty), keyExpansionCommandT), command))
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
                (NewMapObject(keyField, keyT), valueExpression)
              }
            }
          }

          (keyExpansionCommand, valueExpansionExpression) = result
          
          // Really we're updating the key??
          updateKeyTypeResponse <- expandType(keyT, keyExpansionCommand.uObject, env)
          // TODO- we need to do something with updateKeyTypeResponse.converter


          newTableType = MapT(
            env.toTypeTransform(updateKeyTypeResponse.newType, requiredValuesT),
            MapConfig(style, features)
          )
                  
          mapValues <- current.uObject match {
            case UMap(values) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          _ <- Outcome.failWhen(updateKeyTypeResponse.newValues.length > 1, "A Unimplemented")

          newPattern <- Outcome(updateKeyTypeResponse.newValues.headOption, s"Cannot expand type: $keyT and get a new pattern to match")

          prepNewValues = for {
            value <- mapValues

            // Remove old value
            if (value._1 != keyExpansionCommand.uObject)
          } yield (value._1 -> value._2)

          newMapValues = (newPattern -> valueExpansionExpression) +: prepNewValues
        } yield {
          NewMapObject(UMap(newMapValues), newTableType)
        }
      }
      case FunctionalSystemT(functionTypes) => {
        for {
          currentMapping <- current.uObject match {
            case UMap(m) => Success(m)
            case _ => Failure(s"Function not a mapping: ${current.uObject}")
          }

          newFunctionNameObj <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)

          currentFunctionType <- Evaluator.applyFunctionAttempt(UMap(functionTypes), newFunctionNameObj, env)
          currentFunctionT <- env.typeSystem.convertToNewMapType(currentFunctionType)

          currentFunctionTPair <- componentsOfMapType(currentFunctionT, env)
          currentFunctionTypeTransform = currentFunctionTPair._1
          currentFunctionMapConfig = currentFunctionTPair._2

          newFunctionObject <- Evaluator.applyFunctionAttempt(command, UIndex(1), env)

          newFunctionObjectComponents <- newFunctionObject match {
            case UCase(t, UMap(pairs)) => Success(t -> pairs)
            case _ => Failure(s"Recieved Unexpected Object: $newFunctionObject") 
          }

          uNewFunctionType = newFunctionObjectComponents._1
          newFunctionT <- env.typeSystem.convertToNewMapType(uNewFunctionType)

          newFunctionTPair <- componentsOfMapType(newFunctionT, env)
          newFunctionTypeTransform = newFunctionTPair._1
          newFunctionMapConfig = newFunctionTPair._2

          uNewFunctionMapping = newFunctionObjectComponents._2

          composedCompleteness <- if (currentFunctionMapConfig.completeness == newFunctionMapConfig.completeness) {
            Success(currentFunctionMapConfig.completeness)
          } else {
            Failure(s"Completeness doesn't match $currentFunctionMapConfig -- $newFunctionMapConfig")
          }

          composedChannelParentType <- if (currentFunctionMapConfig.channelParentType == newFunctionMapConfig.channelParentType) {
            Success(currentFunctionMapConfig.channelParentType)
          } else {
            Failure(s"channelParentType doesn't match $currentFunctionMapConfig -- $newFunctionMapConfig")
          }
        } yield {
          val composedTypeTransForm = newFunctionTypeTransform ++ currentFunctionTypeTransform

          val composedFeatureSet = if (currentFunctionMapConfig.featureSet.getLevel > newFunctionMapConfig.featureSet.getLevel) {
            currentFunctionMapConfig.featureSet
          } else {
            newFunctionMapConfig.featureSet
          }

          val composedMapConfig = MapConfig(
            composedCompleteness,
            composedFeatureSet,
            currentFunctionMapConfig.preservationRules ++ newFunctionMapConfig.preservationRules,
            newFunctionMapConfig.channels ++ currentFunctionMapConfig.channels,
            composedChannelParentType
          )

          // Object to upgrade the functionalSystemT
          val composedTypeObject = env.typeSystem.typeToUntaggedObject(MapT(UMap(composedTypeTransForm), composedMapConfig))

          // Also upgrade the function itself
          // TODO: I think the composition between uNewFunctionMaping and currentMapping needs to be handled better
          NewMapObject(
            UMap((newFunctionNameObj -> UMap(uNewFunctionMapping)) +: currentMapping),
            FunctionalSystemT((newFunctionNameObj -> composedTypeObject) +: functionTypes)
          )
        }
      }
      case structT@StructT(parameterList, parentFieldType, RequireCompleteness, featureSet) => {
        for {
          mapValues <- current.uObject match {
            case UMap(values) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          nameOfField <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)
          newValueAsNewMapObject <- Evaluator.applyFunctionAttempt(command, UIndex(1), env)

          uCaseValue <- newValueAsNewMapObject match {
            case u@UCase(_, _) => Success(u)
            case _ => Failure(s"Wrong update for complete struct: $newValueAsNewMapObject")
          }
        } yield {
          val typeOfField = uCaseValue.constructor
          val valueOfField = uCaseValue.input

          val newMapValues = (nameOfField -> valueOfField) +: mapValues.filter(x => x._1 != nameOfField)

          val newParams = (nameOfField -> typeOfField) +: parameterList.filter(x => x._1 != nameOfField)

          NewMapObject(UMap(newMapValues), StructT(newParams, parentFieldType, RequireCompleteness, featureSet))
        }
      }
      case StructT(params, parentFieldType, CommandOutput, _) => {
        command match {
          case UCase(constructor, input) => {
            for {
              mapValues <- current.uObject match {
                case UMap(values) => Success(values)
                case _ => Failure(s"Couldn't get map values from $current")
              }

              // TODO - this part is currently only written for _default
              // Params need to be updated!!
              newParams = params
            } yield {
              val newMapValues = (constructor -> input) +: mapValues.filter(x => x._1 != constructor)
              NewMapObject(UMap(newMapValues), StructT(newParams, parentFieldType))
            }
          }
          case _ => {
            Failure(s"A) Structs as commands haven't been implemented yet -- $params -- $command")
          }
        }
      }
      case nType@CustomT("Array", uType) => {

        for {
          untaggedResult <- current.uObject match {
            case UCase(UIndex(length), UStruct(values)) => {
              Success(UCase(UIndex(length + 1), UStruct(values :+ command)))
            }
            case UCase(UIndex(length), UMap(values)) => {
              Success(UCase(UIndex(length + 1), UMap(values :+ (UIndex(length), command))))
            }
            case _ => Failure(s"Unknown array data: $current")
          }

          result <- TypeChecker.tagAndNormalizeObject(untaggedResult, nType, env)
        } yield result
      }
      case nType@CustomT(typeName, params) => {
        val typeSystemId = typeSystemIdOpt.getOrElse(env.typeSystem.currentState)
        val typeSystemMapping = env.typeSystem.historicalMapping.get(typeSystemId).getOrElse(Map.empty) 

        for {
          typeId <- Outcome(typeSystemMapping.get(typeName), s"Couldn't find type: $typeName")
          underlyingTypeInfo <- Outcome(env.typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find type: $typeName -- $typeId")

          (underlyingPattern, underlyingExp) = underlyingTypeInfo

          patternMatchSubstitutions <- Evaluator.attemptPatternMatch(underlyingPattern, params, StandardMatcher, env)

          underlyingType = MakeSubstitution(underlyingExp, patternMatchSubstitutions)

          underlyingT <- env.typeSystem.convertToNewMapType(underlyingType)
          currentResolved <- TypeChecker.tagAndNormalizeObject(current.uObject, underlyingT, env)

          result <- updateVersionedObject(currentResolved, command, env)
          resultResolved <- TypeChecker.tagAndNormalizeObject(result.uObject, nType, env)
        } yield resultResolved
      }
      case WithStateT(typeSystemId, nType) => {
        for {
          retaggedCurrent <- TypeChecker.tagAndNormalizeObject(current.uObject, nType, env)

          result <- updateVersionedObject(retaggedCurrent, command, env, Some(typeSystemId))
        } yield result
      }
      case _ => {
        Failure(s"C) ${current.displayString(env)} is not a command type, error in type checker")
      }
    }
  }

  def componentsOfMapType(
    nType: NewMapType,
    env: Environment
  ): Outcome[(Vector[(UntaggedObject, UntaggedObject)], MapConfig), String] = nType match {
    case MapT(UMap(typeTransform), config) => Success(typeTransform -> config)
    case MapT(UMapPattern(key, value), config) => Success(Vector(key -> value) -> config)
    case UndefinedT => Success(
      Vector.empty,
      MapConfig(RequireCompleteness, BasicMap)
    )
    case _ => Failure(s"Unexpected function type: ${nType.displayString(env)}")
  }

  def checkTypeTransformForSingleBinding(
    typeTransform: UntaggedObject
  ): Outcome[(UntaggedObject, UntaggedObject), String] = typeTransform match {
    case UMap(Vector((key, value))) => Success(key -> value)
    case UMapPattern(key, value) => Success(key -> value)
    case UMap(_) => {
      Failure("Type transform  must have a single binding. Instead it was: " + typeTransform)
    }
    case _ => Failure("Could not handle type transform: " + typeTransform)
  }

  def getTypeTransformBindings(
    typeTransform: UntaggedObject
  ): Outcome[Vector[(UntaggedObject, UntaggedObject)], String] = typeTransform match {
    case UMap(values) => Success(values)
    case UMapPattern(key, value) => Success(Vector(key -> value))
    case _ => Failure("Could not get bindings: " + typeTransform)
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