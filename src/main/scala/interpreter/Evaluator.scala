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
      case AccessField(struct, field) => {
        for {
          evalStruct <- this(struct, env)
          result <- accessFieldAttempt(evalStruct, field, env)
        } yield result
      }
      case ParamId(s) => {
        env.lookup(s) match {
          case None => Failure(s"Unbound identifier: $s")
          case Some(EnvironmentValue(nObject, BoundStatus)) => Success(nObject)
          case Some(EnvironmentValue(nObject, ParameterStatus)) => {
            Failure("sCannot evaluate identifier $s, since it is an unbound parameter of type $nObject")
          }
        }
      }
      case BuildCase(constructor, input, caseType) => {
        for {
          evalInput <- this(input, env)
          untaggedInput <- removeTypeTag(evalInput)
          untaggedConstructor <- removeTypeTag(constructor)
        } yield TaggedObject(UCase(untaggedConstructor, untaggedInput), caseType)
      }
      case BuildMapT(inputType, outputType, completeness, featureSet) => {
        for {
          evalInputType <- this(inputType, env)
          evalOutputType <- this(outputType, env)

          // TODO - this needs to be pushed up to the type checker
          _ <- Outcome.failWhen(
            (completeness == RequireCompleteness) && !RetrieveType.isTermConstant(evalInputType),
            "Cannot build a RequireCompleteness map with an expanding input type. Try adding a required field instead."
          )
        } yield {
          MapT(evalInputType, evalOutputType, completeness, featureSet)
        }
      }
      case BuildTableT(keyType, requiredValues) => {
        for {
          evalKeyType <- this(keyType, env)
          startingType <- getDefaultValueOfCommandType(evalKeyType, env)
          evalRequiredValues <- this(requiredValues, env)
        } yield TableT(startingType, evalRequiredValues)
      }
      case BuildExpandingSubsetT(parentType) => {
        for {
          evalParentType <- this(parentType, env)
        } yield ExpandingSubsetT(evalParentType)
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
        } yield StructT(evalParams)
      }
      case BuildMapInstance(values, nType) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield TaggedObject(UMap(evalValues), nType)
      }
    }
  }

  // TODO - eventually this should go away
  def removeTypeTag(nObject: NewMapObject): Outcome[UntaggedObject, String] = {
    nObject match {
      case TaggedObject(uObject, _) => Success(uObject)
      case VersionedObjectLink(_, _) => Failure("Can't remove type tage from versioned object link")
      case _ => Failure("Can't yet remove type tage from typed object (once types are redefined as a case it'll be possible)")
    }
  }

  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)

  def getDefaultValueOfCommandType(nType: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => Success(Index(0))
      case OrBooleanT => Success(TaggedObject(UIndex(0), OrBooleanT))
      case TableT(_, _) | ExpandingSubsetT(_) | MapT(_, _, CommandOutput, _) => Success(TaggedObject(UMap(Vector.empty), nType))
      case TaggedObject(UIndex(i), _) if i > 0 => Success(TaggedObject(UIndex(0), Index(i)))
      case TypeT => {
        // Maybe there should be a type of cases specifically
        Success(CaseT(
          TaggedObject(
            UMap(Vector.empty),
            MapT(
              IdentifierT,
              TypeT,
              RequireCompleteness,
              BasicMap
            )
          )
        ))
        //Failure("Type of Types has no implemented default value (Maybe it should be empty case)")
      }
      case AnyT => Failure("The \"any\" Type has no implemented default value")
      case IdentifierT => Failure("Type of Identifiers has no default value")
      case MapT(_, _, _, _) => Failure("No default map if not CommandOutput")
      case structT@StructT(TaggedObject(UMap(parameterList), _)) => {
        for {
          defaultValue <- getDefaultValueFromStructParams(parameterList, env)
        } yield {
          TaggedObject(UMap(defaultValue), structT)
        }
      }
      case CaseT(cases) => {
        // In order for cases to have a default value, there's have to be 2 things:
        // - casesType must have a default (a default case) - call it casesType.default
        // - casesToType(casesType.default) is a type that must have a default case
        throw new Exception(s"Case Types do not have a default value -- $nType")
        Failure("Case Types do not have a default value")
      }
      case _ => {
        Failure(s"$nType is not a pure command type, error in type checker")
      }
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
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
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
                RequireCompleteness,
                BasicMap
              )
            )
          )
        }
      }
      case TableT(keyType, requiredValues) => {
        // Key Expansion + requiredValue expansion
        // What if Key expansion is a case? (for now we don't allow this, only basic map)
        for {
          keyExpansionCommandT <- getCommandInputOfCommandType(RetrieveType.fromNewMapObject(keyType, env), env)
        } yield {
          keyExpansionCommandT match {
            case StructT(TaggedObject(UMap(items), _)) if (items.length == 0) => {
              // TODO - this is an ugle exception.. we need a better way to add fields to a struct
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
                  MapT(Index(2), TypeT, RequireCompleteness, BasicMap)
                )
              )
            }
          }
        }
      }
      case ExpandingSubsetT(parentType) => Success(parentType)
      // This should really be a case type instead of a typeT
      case TypeT => {
        Success(
          StructT(
            TaggedObject(
              UMap(Vector(
                ObjectPattern(UIndex(0)) -> ObjectExpression(IdentifierT),
                ObjectPattern(UIndex(1)) -> ObjectExpression(TypeT)
              )),
              MapT(Index(2), TypeT, RequireCompleteness, BasicMap)
            )
          )
        )
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

  def retagPattern(nPattern: NewMapPattern, newTypeTag: NewMapObject): NewMapPattern = nPattern match {
    case ObjectPattern(nObject) => ObjectPattern(nObject)
    case _ => nPattern
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
              newState <- applyFunctionAttempt(IncrementFunc, current, env)
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
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
        for {
          input <- accessFieldAttempt(command, Index(0), env)
          commandForInput <- accessFieldAttempt(command, Index(1), env)

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
      case tableT@TableT(keyType, requiredValues) => {
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
                keyField <- accessFieldAttempt(command, Index(0), env)
                valueField <- accessFieldAttempt(command, Index(1), env)
              } yield (keyField, valueField)
            }
          }

          (keyExpansionCommand, valueExpansionCommand) = result

          updateKeyTypeResponse <- updateVersionedO(keyType, keyExpansionCommand, env)

          newTableType = TableT(updateKeyTypeResponse.newState, requiredValues)
                  
          mapValues <- current match {
            case TaggedObject(UMap(values), _) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          updateKeyUntagged <- removeTypeTag(updateKeyTypeResponse.output)

          newMapping = ObjectPattern(updateKeyUntagged) -> ObjectExpression(valueExpansionCommand)

          prepNewValues = for {
            value <- mapValues
            retaggedPattern = retagPattern(value._1, updateKeyTypeResponse.newState)

            // Remove old value
            if (retaggedPattern != ObjectPattern(updateKeyUntagged))
          } yield (retaggedPattern -> value._2)

          newMapValues = newMapping +: prepNewValues
        } yield UpdateVersionedOResponse(TaggedObject(UMap(newMapValues), newTableType), NewMapO.emptyStruct)
      }
      case ExpandingSubsetT(parentType) => {
        //current is going be of type SubsetT(parentT)
        // and the isMember function is going to have a value of isSubsetOr
        current match {
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
                  MapT(Index(2), TypeT, RequireCompleteness, BasicMap)
                )
              )
            )

            val isMember = TaggedObject(UMap(values), MapT(parentType, OrBooleanT, CommandOutput, BasicMap))

            for {
              result <- updateVersionedO(isMember, isMemberCommand, env)
            } yield {
              // TODO - eventually the output will be different (set of new items??)
              UpdateVersionedOResponse(
                retagObject(result.newState, ExpandingSubsetT(parentType)),
                NewMapO.emptyStruct
              )
            }
          }
          case _ => Failure(s"Didn't get what I expected for ExpandingSubsetT $parentType -- $current -- $command")
        }
      }
      case TypeT => {
        current match {
          case CaseT(TaggedObject(UMap(values), mapT)) => {
            // General map, adding a case
            for {
              newCaseName <- accessFieldAttempt(command, Index(0), env)
              newCaseInputType <- accessFieldAttempt(command, Index(1), env)

              newCaseNameUntagged <- removeTypeTag(newCaseName)
              newMapValues = (ObjectPattern(newCaseNameUntagged) -> ObjectExpression(newCaseInputType)) +: values.filter(x => x._1 != ObjectPattern(newCaseNameUntagged))
            } yield {
              UpdateVersionedOResponse(
                CaseT(TaggedObject(UMap(newMapValues), mapT)),
                newCaseName
              )
            }
          }
          case _ => {
            Failure(s"Cannot expand type $current")
          }
        }
      }
      case structT@StructT(params) => {
        Failure("Structs as commands haven't been implemented yet")
      }
      case CaseT(cases) => {
        Failure("Cases as commands haven't been implemented yet")
      }
      case _ => {
        Failure(s"$current is not a command type, error in type checker")
      }
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
        case _ => Failure(s"Identifier $id does not point to a versioned object. It is actually $versionedObject.")
      }
    } yield versionedO
  }

  def latestVersion(uuid: UUID, env: Environment): Outcome[Long, String] = {
    env.latestVersionNumber.get(uuid) match {
      case Some(v) => Success(v)
      case None => Failure(s"Couldn't find version number for $uuid")
    }
  }

  def currentState(uuid: UUID, env: Environment): Outcome[NewMapObject, String] = {
    for {
      v <- latestVersion(uuid, env)

      currentState <- env.storedVersionedTypes.get(VersionedObjectKey(v, uuid)) match {
        case Some(obj) => Success(obj)
        case None => Failure(s"Couldn't find current state of version $v number for $uuid")
      }
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
        val nps = newParametersFromPattern(k)
        val newEnv = env.newParams(nps)

        // What do we need to do with v?
        // I'd argue nothing - it's already type checked so we know the internal parameters check out
        // This should be left unevaluated!

        // TODO - if this is a basic map, v should be evaluated down to an object
        for {
          evalRest <- evalMapInstanceVals(restOfValues, env)
        } yield {
          (k -> v) +: evalRest
        }
      }
      case _ => Success(Vector.empty)
    }
  }

  // Assume that this is type checked so the field should exist in the struct
  // TODO - I believe this can be merged with apply function attempt!!!
  def accessFieldAttempt(
    struct: NewMapObject,
    field: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    stripVersioning(struct, env) match {
      case TaggedObject(UMap(value), StructT(_)) => {
        attemptPatternMatchInOrder(value, field, env) match {
          case Success(x) => Success(x)
          case Failure(PatternMatchErrorType(message, isEvaluationError)) => Failure(message)
        }
      }
      case CaseT(cases) => {
        for {
          result <- applyFunctionAttempt(cases, field, env)
        } yield {
          val caseInputType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(cases), env)

          // WHAT TO DO HERE!!!
          TaggedObject(
            UMap(Vector(
              TypePattern("input", result) -> BuildCase(field, ParamId("input"), struct)
            )),
            MapT(result, struct, RequireCompleteness, SimpleFunction)
          )
        }
      }
      case _ => Failure(s"Unable to access fields from $struct because it is not a struct")
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
        stripVersioning(nType, env) match {
          case TableT(_, _) => {
            attemptPatternMatchInOrder(values, inputC, env) match {
              case Success(x) => Success(x)
              case Failure(PatternMatchErrorType(message, _)) => Failure(message)
            }
          }
          case MapT(keyType, _, SubtypeInput, _) => {
            // We have a bit of a problem with the subtype input, because the map patterns are tagged with the PARENT TYPE
            // And the inputC is tagged with the SubType
            // This will be fixed when we get rid of subtypes completely.. hopefully.
            // For now - I suppose that a retagging is in order
            // ALSO - shouldn't this be retagged the other way??
            // TODO: REMOVE THIS UGLY CASE
            val retaggedInputC = retagObject(inputC, keyType)

            for {
              keyMatchResult <- attemptPatternMatchInOrder(values, retaggedInputC, env) match {
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
          case MapT(keyType, _, commandType, _) => {
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
          case _ => Failure(s"Cannot apply function of type $nType") 
        }
      }
      case (IsCommandFunc, nObject) => {
        val isCommand: Boolean = getDefaultValueOfCommandType(nObject, env).isSuccess

        Success(Index(if (isCommand) 1 else 0))
      }
      case (IsVersionedFunc, nObject) => Success(
        nObject match {
          case VersionedObjectLink(_, _) => Index(1)
          case _ => Index(0)
        }
      )
      case (IsConstantFunc, nObject) => Success(
        // TODO - I'm not sure this should even be in here -
        //  whether a term is constant should be hidden!
        // Oh well - let's see if we can remove in the future
        if (RetrieveType.isTermConstant(nObject)) {
          Index(1)
        } else {
          Index(0)
        }
      )
      case (IsSimpleFunction, nObject) => {
        nObject match {
          case TaggedObject(_, MapT(_, _, CommandOutput, features)) => {
            if (features == SimpleFunction || features == BasicMap) {
              Success(Index(1))
            } else {
              Success(Index(0))
            }
          }
          case _ => Success(Index(0))
        }
      }
      case (IsSubtypeFunc, nObject) => {
        nObject match {
          case SubtypeT(_) => Success(Index(1))
          case _ => Success(Index(0))
        }        
      }
      case (IncrementFunc, TaggedObject(UIndex(i), nType)) => Success(TaggedObject(UIndex(i + 1), nType))
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
    (pattern, input) match {
      case (StructPattern(params), TaggedObject(UMap(paramValues), StructT(_))) => {
        for {
          inputs <- expressionListToObjects(paramValues.map(_._2), env)
          result <- patternMatchOnStruct(params, inputs, env)
        } yield result 
      }
      // TODO - since input is going to be a literal, do we actually need to call isMemberOfSubtype, or can we
      //  just call the function??
      case (TypePattern(name, nType), _) => {
        for {
          //untaggedInput <- removeTypeTag(input)
          isMember <- SubtypeUtils.isMemberOfSubtype(input /*untaggedInput*/, nType, env)
          _ <- Outcome.failWhen(
            !isMember,
            "Not Member of Subtype"
          )
        } yield Map(name -> input)
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
      case (CasePattern(constructorP, inputP), TaggedObject(UCase(constructor, cInput), CaseT(cases))) => {
        val caseConstructorType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(cases), env)
        val taggedConstructor = TaggedObject(constructor, caseConstructorType)

        for {
          //_ <- Outcome.failWhen(constructorP != taggedConstructor, "Constructors didn't match")
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
  def newParametersFromPattern(pattern: NewMapPattern): Vector[(String, NewMapObject)] = pattern match {
    case ObjectPattern(_) => Vector.empty
    case TypePattern(name, nType) => Vector(name -> nType)
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