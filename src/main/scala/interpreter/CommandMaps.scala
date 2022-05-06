package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Evaluates an expression that's already been type checked
object CommandMaps {
  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)

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
        } yield result
      }
      case _ => {
        Failure("_default doesn't exist yet")
      }
    }
  }

  def getDefaultValueOfCommandType(nType: UntaggedObject, env: Environment): Outcome[UntaggedObject, String] = {
    getDefaultValueOfCommandTypeFromEnv(nType, env).rescue(f => {
      getDefaultValueOfCommandTypeHardcoded(nType, env)
    })
  }

  val defaultUMap = UMap(Vector.empty)

  def getDefaultValueOfCommandTypeHardcoded(nType: UntaggedObject, env: Environment): Outcome[UntaggedObject, String] = {
    nType match {
      // TODO - start removing these in favor of newmap code!
      case UType(IndexT(i)) if i > 0 => Success(UIndex(0))
      case UType(OrBooleanT) => Success(UIndex(0))
      case UType(MapT(_, _, MapConfig(CommandOutput, _, _))) => Success(defaultUMap)
      case UType(MapT(IndexT(0), _, MapConfig(RequireCompleteness, _, _))) => Success(defaultUMap)
      case UType(MapT(SubtypeT(UMap(m), _, _), _, MapConfig(RequireCompleteness, _, _))) => {
        // What if instead of UMap it's ULink?

        if (m.isEmpty) {
          Success(defaultUMap)
        } else {
          Failure(s"Can't start off map with key subtype $m")
        }
        
      }
      //case DataTypeT(_) => Success(defaultUMap)
      case UType(StructT(params, _, _, _)) => {
        // TODO - This is only true if all the type values in params actually have a default
        Success(defaultUMap)
      }
      case UType(CaseT(cases, _, _, _)) => {
        // In order for cases to have a default value, there's have to be 2 things:
        // - casesType must have a default (a default case) - call it casesType.default
        // - casesToType(casesType.default) is a type that must have a default case
        //throw new Exception(s"Case Types do not have a default value -- $nType")
        Failure("Case Types do not have a default value")
      }
      case UType(TypeClassT(typeTransform, typesInTypeClass)) if (typesInTypeClass.isEmpty) => {
        Success(defaultUMap)
      }
      case _ => {
        Failure(s"$nType is not a command type, error in type checker.")
      }
    }
  }

  // Shouldn't this be called expand type?
  def getTypeExpansionCommandInput(
    nType: NewMapType
  ): Outcome[NewMapType, String] = {
    nType match {
      case IndexT(i) => Success(NewMapO.emptyStruct) // Where to insert the new value?
      case CaseT(cases, parentType, featureSet, typeParameters) => {
        Success(StructT(
          Vector(
            ObjectPattern(UIndex(0)) -> ObjectExpression(UType(parentType)),
            ObjectPattern(UIndex(1)) -> ObjectExpression(UType(TypeT))
          ),
          IndexT(2)
        ))
      }
      case StructT(cases, parentType, featureSet, typeParameters) => {
        Success(StructT(
          Vector(
            ObjectPattern(UIndex(0)) -> ObjectExpression(UType(parentType)),
            ObjectPattern(UIndex(1)) -> ObjectExpression(UType(SubtypeT(IsCommandFunc, TypeT)))
          ),
          IndexT(2)
        ))
      }
      case SubtypeT(isMember, parentType, featureSet) => Success(parentType)
      case MapT(keyType, valueType, config) => getTypeExpansionCommandInput(valueType)
      case CustomT(uuid, nType) => getTypeExpansionCommandInput(nType)
      case _ => Failure(s"Unable to expand key: $nType")
    }
  }

  val untaggedIdentity: UntaggedObject = UMap(Vector(WildcardPattern("_") -> ParamId("_")))

  case class ExpandKeyResponse(
    newType: NewMapType,
    newValueOpt: Option[NewMapPattern],
    converter: UntaggedObject // This is a function that can convert from the old type to the new type
  )

  def expandType(
    nType: NewMapType,
    command: UntaggedObject,
    env: Environment
  ): Outcome[ExpandKeyResponse, String] = {
    nType match {
      case IndexT(i) => {
        val newType = IndexT(i + 1)
        Success(ExpandKeyResponse(IndexT(i + 1), Some(ObjectPattern(UIndex(i))), untaggedIdentity))
      }
      case CaseT(cases, parentType, featureSet, typeParameters) => {
        val uConstructors = cases.map(x => x._1 -> ObjectExpression(UIndex(1)))
        val constructorsSubtype = SubtypeT(UMap(uConstructors), parentType, featureSet)
        val caseMap = TaggedObject(UMap(cases), MapT(constructorsSubtype, TypeT, MapConfig(RequireCompleteness, BasicMap)))

        for{
          newCaseMap <- updateVersionedObject(caseMap, command, env)
          untaggedNewCaseMap <- Evaluator.removeTypeTag(newCaseMap)

          newCaseName <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)
        } yield {
          untaggedNewCaseMap match {
            case UMap(newCases) => {
              ExpandKeyResponse(
                CaseT(newCases, parentType, featureSet, typeParameters),
                Some(CasePattern(newCaseName, WildcardPattern("_"))),
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
        val isMemberMap = TaggedObject(isMember, MapT(parentType, OrBooleanT, MapConfig(CommandOutput, BasicMap)))

        val adjustedCommand = UMap(Vector(
          ObjectPattern(UIndex(0)) -> ObjectExpression(command),
          ObjectPattern(UIndex(1)) -> ObjectExpression(UIndex(1))
        ))

        for {
          newMembersMap <- updateVersionedObject(isMemberMap, adjustedCommand, env)
          untaggedNewMembersMap <- Evaluator.removeTypeTag(newMembersMap)
          fixedUntaggedNewMembersMap = untaggedNewMembersMap match {
            case UMap(values) => UMap(values :+ (WildcardPattern("_") -> ObjectExpression(UIndex(0))))
            case _ => untaggedNewMembersMap
          }
        } yield {
          ExpandKeyResponse(
            SubtypeT(fixedUntaggedNewMembersMap, parentType, featureSet),
            Some(ObjectPattern(command)),
            untaggedIdentity
          )
        }
      }
      case MapT(keyType, valueType, config) => {
        for {
          expandedValueInfo <- expandType(valueType, command, env)
        } yield {
          val newType = MapT(keyType, expandedValueInfo.newType, config)

          // TODO - the valueTypes need to be converted - figure out a way to do this!!
          // - Sort of an implementation of mapValues
          //expandedValeInfo.converter
          
          ExpandKeyResponse(nType, None, untaggedIdentity)
        }
      }
      case CustomT(uuid, nType) => {
        expandType(nType, command, env)

        // TODO - store converter in the environment?
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
      case OrBooleanT => Success(IndexT(2))
      case MapT(inputType, outputType, MapConfig(CommandOutput, _, _)) => {
        // TO incorporate TableT:
        // Look at input type
        // See if input type is itself a command type
        // Then, if it is - include that in the command input!

        for {
          outputCommandT <- getCommandInputOfCommandType(outputType, env)
        } yield {
          StructT(
            Vector(
              ObjectPattern(UIndex(0)) -> ObjectExpression(UType(inputType)),
              ObjectPattern(UIndex(1)) -> ObjectExpression(UType(outputCommandT))
            ),
            IndexT(2)
          )
        }
      }
      case MapT(keyType, requiredValues, _) => {
        // In this case, there must be a key expansion type
        // TODO: enforce this?

        // Key Expansion + requiredValue expansion
        // What if Key expansion is a case? (for now we don't allow this, only basic map)
        for {
          keyExpansionCommandT <- getTypeExpansionCommandInput(keyType)
        } yield {
          keyExpansionCommandT match {
            case StructT(items, _, _, _) if (items.length == 0) => {
              // TODO - this is an ugly exception.. we need a better way to add fields to a struct
              // (particularly an empty struct like in this case)
              requiredValues
            }
            case _ => {
              StructT(
                Vector(
                  ObjectPattern(UIndex(0)) -> ObjectExpression(UType(keyExpansionCommandT)),
                  ObjectPattern(UIndex(1)) -> ObjectExpression(UType(requiredValues))
                ),
                IndexT(2)
              )
            }
          }
        }
      }
      case structT@StructT(parameterList, parentFieldType, featureSet, typeParameters) => {
        // Change to CaseT because we are adding a single parameter!
        // Are we allowed to change an old parameter? Let's say sure.
        Success(CaseT(parameterList, parentFieldType, featureSet, typeParameters))
      }
      case CaseT(cases, _, _, _) => {
        Failure("Cases as commands haven't been implemented yet")
      }
      case TypeClassT(typeTransform, typesInTypeClass) => {
        Success(CaseT(typeTransform, TypeT, SimpleFunction))
      }
      case _ => {
        Failure(s"$nType is not a command type, error in type checker")
      }
    }
  }

  // This is a weird artifact needed for updateVersionedO.. it's going to improve
  // By having objects tagged the old way automatically updated to the new way
  /*def retagObject(nObject: NewMapObject, newTypeTag: NewMapObject): NewMapObject = {
    nObject match {
      case TaggedObject(untagged, nType) => TaggedObject(untagged, newTypeTag)
      case _ => nObject
    }
  }*/

  def updateVersionedObject(
    current: NewMapObject,
    command: UntaggedObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    RetrieveType.fromNewMapObject(current, env) match {
      case CountT => {
        current match {
          case TaggedObject(c@UIndex(i), nType) => {
            for {
              newState <- Evaluator.applyFunctionAttempt(IncrementFunc, c, env)
            } yield {
              TaggedObject(newState, nType)
            }
          }
          case _ => {
            throw new Exception("Invalid count in versioning upgrade")
          }
        }
      }
      case OrBooleanT => {
        (current, command) match {
          case (TaggedObject(UIndex(i), _), UIndex(j)) => {
            val result = if (i == 1 || j == 1) 1 else 0
            Success(
              TaggedObject(UIndex(result), OrBooleanT),
            )
          }
        case _ => Failure("This didn't work")
        }
      }
      case mapT@MapT(inputType, outputType, MapConfig(CommandOutput, featureSet, _)) => {
       for {
          input <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)
          commandForInput <- Evaluator.applyFunctionAttempt(command, UIndex(1), env)

          untaggedCurrent <- Evaluator.removeTypeTag(current)

          defaultValue <- CommandMaps.getDefaultValueOfCommandType(UType(outputType), env)

          fixedUntaggedCurrent = untaggedCurrent match {
            case UMap(values) => UMap(values :+ (WildcardPattern("_") -> ObjectExpression(defaultValue)))
            case _ => untaggedCurrent
          }

          currentResultForInput <- Evaluator.applyFunctionAttempt(fixedUntaggedCurrent, input, env)

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

          newMapValues = (ObjectPattern(input) -> ObjectExpression(untaggedNewState)) +: mapValues.filter(x => x._1 != ObjectPattern(input))
        } yield {
          TaggedObject(UMap(newMapValues), mapT)
        }
      }
      case mapT@MapT(keyType, requiredValues, MapConfig(style, features, _)) => {
        for {
          keyExpansionCommandT <- getTypeExpansionCommandInput(keyType)

          result <- keyExpansionCommandT match {
            case StructT(items, _, _, _) if (items.length == 0) => {
              // TODO - this is an ugle exception.. we need a better way to add fields to a struct
              // (particularly an empty struct like in this case)
              Success((TaggedObject(UMap(Vector.empty), keyExpansionCommandT), command))
            }
            case _ => {
              for {
                keyField <- Evaluator.applyFunctionAttempt(command, UIndex(0), env)
                valueField <- Evaluator.applyFunctionAttempt(command, UIndex(1), env)
              } yield (TaggedObject(keyField, keyType), valueField)
            }
          }

          (keyExpansionCommand, valueExpansionCommand) = result

          updateKeyUntagged <- Evaluator.removeTypeTag(keyExpansionCommand)
          
          // Really we're updating the key??
          updateKeyTypeResponse <- expandType(keyType, updateKeyUntagged, env)
          // TODO- we need to do something with updateKeyTypeResponse.converter


          newTableType = MapT(updateKeyTypeResponse.newType, requiredValues, MapConfig(style, features))
                  
          mapValues <- current match {
            case TaggedObject(UMap(values), _) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          newPattern <- Outcome(updateKeyTypeResponse.newValueOpt, "Cannot expand type: $keyType and get a new pattern to match")

          newMapping = newPattern -> ObjectExpression(valueExpansionCommand)

          prepNewValues = for {
            value <- mapValues

            // Remove old value
            if (value._1 != ObjectPattern(updateKeyUntagged))
          } yield (value._1 -> value._2)

          newMapValues = newMapping +: prepNewValues
        } yield {
          TaggedObject(UMap(newMapValues), newTableType)
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
              val newMapValues = (ObjectPattern(constructor) -> ObjectExpression(input)) +: mapValues.filter(x => x._1 != ObjectPattern(constructor))
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
              val newTypesInClass = ObjectPattern(constructor) +: typesInTypeClass.filter(x => x != ObjectPattern(constructor))
              val newMapValues = (ObjectPattern(constructor) -> ObjectExpression(input)) +: mapValues.filter(x => x._1 != ObjectPattern(constructor))
              TaggedObject(UMap(newMapValues), TypeClassT(typeTransform, newTypesInClass))
            }
          }
          case _ => {
            Failure(s"Wrong input for typeClassT -- $current -- $command")
          }
        }
      }
      case _ => {
        Failure(s"$current is not a command type, error in type checker")
      }
    }
  }

  def getDefaultValueFromStructParams(
    params: Vector[(NewMapPattern, NewMapExpression)],
    env: Environment
  ): Outcome[Vector[(NewMapPattern, NewMapExpression)], String] = {
    params match {
      case (fieldName, typeOfFieldExp) +: restOfParams => {
        for {
          typeOfField <- Evaluator(typeOfFieldExp, env)
          paramDefault <- getDefaultValueOfCommandType(typeOfField, env)
          restOfParamsDefault <- getDefaultValueFromStructParams(restOfParams, env)
        } yield {
          (fieldName -> ObjectExpression(paramDefault)) +: restOfParamsDefault
        }
      }
      case _ => Success(Vector.empty)
    }
  }
}