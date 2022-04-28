package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Evaluates an expression that's already been type checked
object Evaluator {
  def apply(
    nExpression: NewMapExpression,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    nExpression match {
      case ObjectExpression(nObject) => Success(nObject)
      case ApplyFunction(func, input) => {
        for {
          evalFunc <- this(func, env)
          evalInput <- this(input, env)
          result <- applyFunctionAttempt(evalFunc, evalInput, env)
        } yield {
          result
        }
      }
      case ParamId(s) => {
        env.lookup(s) match {
          case None => Failure(s"Unbound identifier: $s")
          case Some(EnvironmentValue(nObject, BoundStatus)) => Success(nObject)
          case Some(EnvironmentValue(nObject, ParameterStatus)) => {
            //throw new Exception(s"Cannot evaluate identifier $s, since it is an unbound parameter of type $nObject")
            Failure(s"Cannot evaluate identifier $s, since it is an unbound parameter of type $nObject")
          }
        }
      }
      case BuildCase(constructor, input, caseType) => {
        for {
          evalInput <- this(input, env)
          untaggedInput <- removeTypeTag(evalInput)
          untaggedConstructor <- removeTypeTag(constructor)
        } yield {
          TaggedObject(UCase(untaggedConstructor, untaggedInput), caseType)
        }
      }
      case BuildMapT(inputType, outputType, config) => {
        for {
          evalInputType <- this(inputType, env)
          evalOutputType <- this(outputType, env)
        } yield {
          MapT(evalInputType, evalOutputType, config)
        }
      }
      case BuildTableT(keyType, requiredValues) => {
        for {
          evalKeyType <- this(keyType, env)
          startingType <- getDefaultValueOfCommandType(evalKeyType, env)
          evalRequiredValues <- this(requiredValues, env)
        } yield {
          // TODO - are we going to know that this is an expandable type?
          // - I think so because startingType is tagged!!
          MapT(startingType, evalRequiredValues, MapConfig(RequireCompleteness, SimpleFunction))
        }
      }
      case BuildExpandingSubsetT(parentType, allowPatternMatching) => {
        for {
          evalParentType <- this(parentType, env)
        } yield ExpandingSubsetT(evalParentType, allowPatternMatching)
      }
      case BuildSubtypeT(isMember) => {
        for {
          evalIsMember <- this(isMember, env)
        } yield SubtypeT(evalIsMember)
      }
      case BuildCaseT(cases) => {
        for {
          evalCases <- this(cases, env)
        } yield CaseT(evalCases)
      }
      case BuildStructT(params) => {
        for {
          evalParams <- this(params, env)
        } yield {
          StructT(evalParams)
        }
      }
      case BuildMapInstance(values, nType) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield TaggedObject(UMap(values), nType)
      }
    }
  }

  // TODO - eventually this should go away
  def removeTypeTag(nObject: NewMapObject): Outcome[UntaggedObject, String] = {
    nObject match {
      case TaggedObject(uObject, _) => Success(uObject)
      case VersionedObjectLink(_, _) => Failure(s"Can't remove type tage from versioned object link")
      case CountT | TypeT | AnyT | MapT(_, _, _) | StructT(_) | CaseT(_) | OrBooleanT | IdentifierT | ExpandingSubsetT(_, _) | SubtypeT(_) => Success(UType(nObject))
      case _ => {
        //throw new Exception(nObject.toString)
        Failure(s"Can't yet remove type tag from typed object $nObject (once types are redefined as a case it'll be possible)")
      }
    }
  }

  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)

  /*
   * This is getDefaultValueOfCommandType being slowly written into newmap code
   */
  def getDefaultValueOfCommandTypeFromEnv(nType: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    env.lookup("_default") match {
      case Some(EnvironmentValue(defaultMap, BoundStatus)) => {
        for {
          mapValues <- stripVersioning(defaultMap, env) match {
            case TaggedObject(UMap(values), _) => Success(values)
            case _ => Failure("_default doesn't look the way we expect")
          }
          
          // I wanted to call "applyFunctionAttempt" here, but we can't call getDefaultValueOfCommandType
          // otherwise, we get an infinite loop
          result <- attemptPatternMatchInOrder(mapValues, stripVersioning(nType, env), env) match {
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
      case TypeT => {
        // TODO - maybe this shouldn't be TypeT, but just a Case subtype?
        for {
          emptyCase <- getDefaultValueOfCommandType(MapT(
            TaggedObject(UMap(Vector.empty), ExpandingSubsetT(IdentifierT, false)),
            TypeT,
            MapConfig(RequireCompleteness, BasicMap)
          ), env)
        } yield CaseT(emptyCase)
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
    stripVersioning(nType, env) match {
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
      case TypeT => {
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

        val keyTypeC = stripVersioning(keyType, env)

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
              newState <- applyFunctionAttempt(
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
          input <- applyFunctionAttempt(command, Index(0), env)
          commandForInput <- applyFunctionAttempt(command, Index(1), env)

          currentResultForInput <- applyFunctionAttempt(current, input, env)

          newResultForInput <- updateVersionedO(currentResultForInput, commandForInput, env)

          mapValues <- current match {
            case TaggedObject(UMap(values), _) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          untaggedInput <- removeTypeTag(input)

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
                keyField <- applyFunctionAttempt(command, Index(0), env)
                valueField <- applyFunctionAttempt(command, Index(1), env)
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

          updateKeyUntagged <- removeTypeTag(updateKeyTypeResponse.output)

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
        stripVersioning(current, env) match {
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
      case TypeT => {
        current match {
          case CaseT(inside@TaggedObject(UMap(values), mapT)) => {
            for{
              result <- updateVersionedO(inside, command, env)
            } yield {
              UpdateVersionedOResponse(
                CaseT(result.newState),
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
              taggedConstructor = noramlizeTypeTaggedObject(TaggedObject(constructor, RetrieveType.getParentType(caseConstructorType, env)))

              typeForInput <- applyFunctionAttempt(params, taggedConstructor, env)
              taggedInput = noramlizeTypeTaggedObject(TaggedObject(input, typeForInput))

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

  // This is neccesary for when we have types as tagged objects and types and standalones.
  // Once types are always tagged objects - we should be able to remove this
  def noramlizeTypeTaggedObject(nObject: NewMapObject): NewMapObject = {
    nObject match {
      case TaggedObject(UType(t), TypeT) => t
      case _ => nObject
    }
  }

  def lookupVersionedObject(
    id: String,
    env: Environment
  ): Outcome[VersionedObjectLink, String] = {
    for {
      versionedObject <- Outcome(env.lookup(id), s"Identifier $id not found!")

      _ <- Outcome.failWhen(versionedObject.status != BoundStatus, s"Identifier $id is a parameter, should be an object")

      versionedO <- versionedObject.nObject match {
        case vo@VersionedObjectLink(_, _) => Success(vo)
        case _ => Failure(s"Identifier $id does not point to a versioned object. It is actually ${versionedObject.nObject}.")
      }
    } yield versionedO
  }

  def latestVersion(uuid: UUID, env: Environment): Outcome[Long, String] = {
    env.latestVersionNumber.get(uuid) match {
      case Some(v) => Success(v)
      case None => Failure(s"Couldn't find version number for $uuid")
    }
  }

  def indicatedState(key: VersionedObjectKey, env: Environment): Outcome[NewMapObject, String] = {
    for {
      currentState <- env.storedVersionedTypes.get(key) match {
        case Some(obj) => Success(obj)
        case None => Failure(s"Couldn't find current state of version ${key.versionNumber} number for ${key.uuid}")
      }
    } yield currentState
  }

  def currentState(uuid: UUID, env: Environment): Outcome[NewMapObject, String] = {
    for {
      v <- latestVersion(uuid, env)
      currentState <- indicatedState(VersionedObjectKey(v, uuid), env)
    } yield currentState
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
      versionLink <- lookupVersionedObject(id, env)
      latestVersion <- latestVersion(versionLink.key.uuid, env)
      currentState <- currentState(versionLink.key.uuid, env)
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

  def evalMapInstanceVals(
    values: Vector[(NewMapPattern, NewMapExpression)],
    env: Environment
  ): Outcome[Vector[(NewMapPattern, NewMapExpression)], String] = {
    values match {
      case (k, v) +: restOfValues => {
        for {
          // If this is a basic map element, v should be evaluated down to an object
          newV <- if (newParametersFromPattern(k).isEmpty) {
            Evaluator(v, env).map(vObj => ObjectExpression(vObj))
          } else Success(v)

          evalRest <- evalMapInstanceVals(restOfValues, env)
        } yield {
          (k -> v) +: evalRest
        }
      }
      case _ => Success(Vector.empty)
    }
  }

  def expressionListToObjects(
    nExpressions: Vector[NewMapExpression],
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = {
    nExpressions match {
      case nExpression +: tailExpressions => {
        for {
          nObject <- this(nExpression, env)
          restOfObjects <- expressionListToObjects(tailExpressions, env)
        } yield nObject +: restOfObjects
      }
      case _ => Success(Vector.empty)
    }
  }


  // Assume that both the function and the input have been evaluated
  def applyFunctionAttempt(
    func: NewMapObject,
    input: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    // TODO - is there a way to know if the input has already been evaluated as much as possible
    //Evaluate(input, env)
    // TODO - can we make sure that we get the Current Constant Value BEFORE this is called?
    val funcC = stripVersioning(func, env)
    val inputC = stripVersioning(input, env)

    (funcC, inputC) match {
      case (TaggedObject(UMap(values), nType), _) => {
        for {
          keyMatchResult <- attemptPatternMatchInOrder(values, inputC, env) match {
            case Success(result) => Success(result)
            case Failure(PatternMatchErrorType(message, isEvaluationError)) => {
              if (isEvaluationError) {
                Failure(message)
              } else {
                // Because this is already type checked, we can infer that MapCompleteness == CommandOutput
                // - If it had equaled "MapCompleteness", then we shouldn't be in a situation with no match
                // TODO - instead of calling "RetrieveType" on the full object, we should look at the output type of Func,
                //  and get the default from that
                val typeOfFunction = RetrieveType.fromNewMapObject(func, env)

                val nType = RetrieveType.retrieveOutputTypeFromFunctionType(typeOfFunction, env)
                
                getDefaultValueOfCommandType(RetrieveType.getParentType(nType, env), env)
              }
            }
          }
        } yield {
          keyMatchResult
        }
      }
      case (TaggedObject(IsCommandFunc, _), nObject) => {
        val isCommand: Boolean = getDefaultValueOfCommandType(nObject, env).isSuccess

        Success(Index(if (isCommand) 1 else 0))
      }
      case (TaggedObject(IsSimpleFunction, _), nObject) => {
        nObject match {
          case TaggedObject(_, MapT(_, _, MapConfig(CommandOutput, features, _, _))) => {
            if (features == SimpleFunction || features == BasicMap) {
              Success(Index(1))
            } else {
              Success(Index(0))
            }
          }
          case _ => Success(Index(0))
        }
      }
      case (TaggedObject(IncrementFunc, _), TaggedObject(UIndex(i), nType)) => Success(TaggedObject(UIndex(i + 1), nType))
      case _ => {
        Failure(s"Not implemented: apply function\nCallable: $func\nInput: $input")
      }
    }
  }

  // TODO - there should be no evaluation errors
  // This is only to catch non-constant values, which cannot be checked by the type checker yet
  case class PatternMatchErrorType(message: String, isEvaluationError: Boolean)

  def attemptPatternMatchInOrder(
    remainingPatterns: Vector[(NewMapPattern, NewMapExpression)],
    input: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, PatternMatchErrorType] = {
    remainingPatterns match {
      case (pattern, answer) +: addlPatterns => {
        attemptPatternMatch(pattern, input, env) match {
          case Success(paramsToSubsitute) => {
            this(MakeSubstitution(answer, paramsToSubsitute, env), env) match {
              case Success(nObject) => Success(nObject)
              case Failure(s) => Failure(PatternMatchErrorType(s, true))
            }
          }
          case Failure(_) => attemptPatternMatchInOrder(addlPatterns, input, env)
        }
      }
      case _ => Failure(PatternMatchErrorType(
        s"Unable to pattern match $input, The type checker should have caught this so there may be an error in there",
        false
      ))
    }
  }

  // The input must be a literal input (or at least the first layer cannot be a parameter)
  def attemptPatternMatch(
    pattern: NewMapPattern,
    input: NewMapObject,
    env: Environment
  ): Outcome[Map[String, NewMapObject], String] = {
    // TODO: IMPORTANT
    // We must be able to dea with using the same variable in a pattern, like StructPattern(x, x) to
    //  denote that these are the same
    (pattern, stripVersioning(input, env)) match {
      case (StructPattern(params), TaggedObject(UMap(paramValues), StructT(_))) => {
        for {
          inputs <- expressionListToObjects(paramValues.map(_._2), env)
          result <- patternMatchOnStruct(params, inputs, env)
        } yield result 
      }
      // TODO - since input is going to be a literal, do we actually need to call isMemberOfSubtype, or can we
      //  just call the function??
      case (WildcardPattern(name), _) => {
        Success(Map(name -> input))
      }
      // TODO - eventually instead of checking equality, we'll check for "convertability"
      //  For example between different type versions
      case (ObjectPattern(oPattern), _) => {
        // TODO - the retagging should not happen here
        // (In fact, at this point we should have harmonized the types)
        //val retaggedInput = retagObject(input, RetrieveType.fromNewMapObject(oPattern, env))
        val untaggedInput = removeTypeTag(input).toOption.get

        if (oPattern == untaggedInput) {
          Success(Map.empty)
        } else Failure("ObjectPattern didn't match")
      }
      case (CasePattern(constructorP, inputP), TaggedObject(UCase(constructor, cInput), inputType)) => {
        for {
          cases <- stripVersioning(inputType, env) match {
            case CaseT(params) => Success(params)
            case _ => Failure(s"Unexpected case type: $inputType")
          }

          caseConstructorType = RetrieveType.retrieveInputTypeFromFunctionObj(cases, env)
          taggedConstructor = TaggedObject(constructor, caseConstructorType)

          _ <- Outcome.failWhen(constructorP != constructor, "Constructors didn't match")

          typeOfCInput <- applyFunctionAttempt(cases, taggedConstructor, env)

          result <- attemptPatternMatch(inputP, TaggedObject(cInput, typeOfCInput), env)
        } yield result
      }
      case _ => {
        Failure("Failed Pattern Match")
      }
    }
  }

  def patternMatchOnStruct(
    structPattern: Vector[NewMapPattern],
    inputs: Vector[NewMapObject],
    env: Environment
  ): Outcome[Map[String, NewMapObject], String] = {
    (structPattern, inputs) match {
      case (firstPattern +: restOfPatterns, firstInput +: restOfInputs) => {
        for {
          newParameters <- attemptPatternMatch(firstPattern, firstInput, env)
          otherParameters <- patternMatchOnStruct(restOfPatterns, restOfInputs, env)
        } yield {
          newParameters ++ otherParameters
        }
      }
      case _ => Success(Map.empty) // No patterns to match
    }
  }

  // TODO - move this elsewhere, maybe to environment!
  def newParametersFromPattern(pattern: NewMapPattern): Vector[String] = pattern match {
    case ObjectPattern(_) => Vector.empty
    case WildcardPattern(name) => Vector(name)
    case StructPattern(patterns) => patterns match {
      case firstPattern +: otherPatterns => {
        newParametersFromPattern(firstPattern) ++ newParametersFromPattern(StructPattern(otherPatterns))
      }
      case _ => Vector.empty
    }
    case CasePattern(constructor, input) => {
      newParametersFromPattern(input)
    }
  }

  def stripVersioning(nObject: NewMapObject, env: Environment): NewMapObject = {
    nObject match {
      case VersionedObjectLink(key, status) => {
        // TODO - make this function an outcome
        currentState(key.uuid, env).toOption.get
      }
      case _ => nObject
    }
  }
}