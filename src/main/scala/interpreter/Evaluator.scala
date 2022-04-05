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
        } yield CaseInstance(constructor, evalInput, caseType)
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
      case BuildSeqT(underlyingType) => {
        for {
          evalUnderlyingType <- this(underlyingType, env)
        } yield SequenceT(evalUnderlyingType)
      }
      case BuildTableT(keyType, requiredValues) => {
        for {
          evalKeyType <- this(keyType, env)
          evalRequiredValues <- this(requiredValues, env)
        } yield TableT(evalKeyType, evalRequiredValues)
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
      case BuildMapInstance(values, mapT) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield MapInstance(evalValues, mapT)
      }
      case BuildStructInstance(values, structT) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield StructInstance(evalValues, structT)
      }
      case BuildSeqInstance(values, sequenceT) => {
        for {
          evalValues <- evalSeqInstanceVals(values, env)
        } yield SequenceInstance(evalValues, sequenceT)
      }
      case BuildTableInstance(values, tableT) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield TableInstance(evalValues, tableT)
      }
    }
  }

  def getDefaultValueOfCommandType(nType: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => Success(Index(0))
      case seqT@SequenceT(nType) => Success(SequenceInstance(Vector.empty, seqT))
      case tableT@TableT(keyType, requiredValues) => Success(TableInstance(Vector.empty, tableT))
      case Index(i) if i > 0 => Success(IndexValue(0, Index(i)))
      case TypeT => {
        // Maybe there should be a type of cases specifically
        Success(CaseT(
          MapInstance(
            Vector.empty,
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
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
        Success(MapInstance(Vector.empty, mapT))
      }
      case MapT(_, _, _, _) => Failure("No default map if not CommandOutput")
      case structT@StructT(MapInstance(parameterList, _)) => {
        for {
          defaultValue <- getDefaultValueFromStructParams(parameterList, env)
        } yield {
          StructInstance(defaultValue, structT)
        }
      }
      case CaseT(cases) => {
        // In order for cases to have a default value, there's have to be 2 things:
        // - casesType must have a default (a default case) - call it casesType.default
        // - casesToType(casesType.default) is a type that must have a default case
        Failure("Case Types do not have a default value")
      }
      case _ => {
        Failure(s"$nType is not a pure command type, error in type checker")
      }
    }
  }

  def getCommandInputOfPureCommandType(
    nType: NewMapObject
  ): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => Success(
        NewMapO.emptyStruct
      )
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
        for {
          outputCommandT <- getCommandInputOfPureCommandType(outputType)
        } yield {
          StructT(
            MapInstance(
              Vector(ObjectPattern(Index(0)) -> ObjectExpression(inputType), ObjectPattern(Index(1)) -> ObjectExpression(outputCommandT)),
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
      case SequenceT(nType) => Success(nType)
      case TableT(keyType, requiredValues) => {
        // Key Expansion + requiredValue expansion
        // What if Key expansion is a case? (for now we don't allow this, only basic map)
        for {
          keyExpansionCommandT <- getCommandInputOfPureCommandType(keyType)
        } yield {
          StructT(
            MapInstance(
              Vector(ObjectPattern(Index(0)) -> ObjectExpression(keyExpansionCommandT), ObjectPattern(Index(1)) -> ObjectExpression(requiredValues)),
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
      // This should really be a case type instead of a typeT
      case TypeT => {
        Success(
          StructT(
            MapInstance(
              Vector(ObjectPattern(Index(0)) -> ObjectExpression(IdentifierT), ObjectPattern(Index(1)) -> ObjectExpression(TypeT)),
              MapT(
                Index(2),
                TypeT,
                RequireCompleteness,
                BasicMap
              )
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
        Failure(s"$nType is not a command type, error in type checker")
      }
    }
  }

  def updateVersionedO(
    current: NewMapObject,
    command: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    RetrieveType.fromNewMapObject(current, env) match {
      case CountT => applyFunctionAttempt(IncrementFunc, current, env)
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
        for {
          input <- accessFieldAttempt(command, Index(0), env)
          commandForInput <- accessFieldAttempt(command, Index(1), env)

          currentResultForInput <- applyFunctionAttempt(current, input, env)

          newResultForInput <- updateVersionedO(currentResultForInput, commandForInput, env)

          mapValues <- current match {
            case MapInstance(values, _) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          newMapValues = (ObjectPattern(input) -> ObjectExpression(newResultForInput)) +: mapValues.filter(x => x._1 != ObjectPattern(input))
        } yield {
          MapInstance(newMapValues, mapT)
        }
      }
      case seqT@SequenceT(nType) => {
        for {
          seqValues <- current match {
            case SequenceInstance(values, seqT) => Success(values)
            case _ => Failure(s"Couldn't get seq values from $current")
          }
        } yield SequenceInstance(seqValues :+ command, seqT)
      }
      case tableT@TableT(keyType, requiredValues) => {
        for {
          keyExpansionCommand <- accessFieldAttempt(command, Index(0), env)
          valueExpansionCommand <- accessFieldAttempt(command, Index(1), env)

          mapValues <- current match {
            case TableInstance(values, _) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          newMapValues = (ObjectPattern(keyExpansionCommand) -> ObjectExpression(valueExpansionCommand)) +: mapValues.filter(x => x._1 != ObjectPattern(keyExpansionCommand))
        } yield TableInstance(newMapValues, tableT)
      }
      case TypeT => {
        current match {
          case CaseT(MapInstance(values, mapT)) => {
            // General map, adding a case
            for {
              newCaseName <- accessFieldAttempt(command, Index(0), env)
              newCaseInputType <- accessFieldAttempt(command, Index(1), env)

              newMapValues = (ObjectPattern(newCaseName) -> ObjectExpression(newCaseInputType)) +: values.filter(x => x._1 != ObjectPattern(newCaseName))
            } yield {
              CaseT(MapInstance(newMapValues, mapT))
            }
          }
          case _ => {
            Failure("Cannot expand type $current")
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
    newValue: NewMapObject
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
      newState <- updateVersionedO(currentState, command, env)
    } yield {
      UpdateVersionedObjectResponse(latestVersion + 1, versionLink.key.uuid, newState)
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

  def evalSeqInstanceVals(
    values: Vector[NewMapExpression],
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = values match {
    case v +: restOfValues => {
      for {
        evalV <- this(v, env)
        evalRest <- evalSeqInstanceVals(restOfValues, env)
      } yield {
        evalV +: evalRest
      }
    }
    case _ => Success(Vector.empty)
  }

  // Assume that this is type checked so the field should exist in the struct
  // TODO - I believe this can be merged with apply function attempt!!!
  def accessFieldAttempt(
    struct: NewMapObject,
    field: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    stripVersioning(struct, env) match {
      case StructInstance(value: Vector[(NewMapPattern, NewMapExpression)], structT) => {
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
          MapInstance(
            Vector(
              TypePattern("input", result) -> BuildCase(field, ParamId("input"), struct)
            ),
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
      case (MapInstance(values, _), key) => {
        for {
          keyMatchResult <- attemptPatternMatchInOrder(values, key, env) match {
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
      // TODO: sequences should have their own indecies rather than checking values.length
      case (SequenceInstance(values, seqT), IndexValue(i, Index(j))) if (j == values.length) => {
        Success(values(i.toInt))
      }
      case (TableInstance(values, seqT), input) => {
        attemptPatternMatchInOrder(values, input, env) match {
          case Success(x) => Success(x)
          case Failure(PatternMatchErrorType(message, _)) => Failure(message)
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
          case MapInstance(_, MapT(_, _, CommandOutput, features)) => {
            if (features == SimpleFunction || features == BasicMap) {
              Success(Index(1))
            } else {
              Success(Index(0))
            }
          }
          case _ => Success(Index(0))
        }
      }
      case (IncrementFunc, Index(i)) => Success(Index(i + 1))
      /*case (AccessField(caseT@CaseT(_), field), _) => {
        Success(AbleToApplyFunction(CaseInstance(field, input, caseT)))
      }*/
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
      case (StructPattern(params), StructInstance(paramValues, _)) => {
        for {
          inputs <- expressionListToObjects(paramValues.map(_._2), env)
          result <- patternMatchOnStruct(params, inputs, env)
        } yield result 
      }
      // TODO - since input is going to be a literal, do we actually need to call isMemberOfSubtype, or can we
      //  just call the function??
      case (TypePattern(name, nType), _) if SubtypeUtils.isMemberOfSubtype(input, nType, env).toOption.getOrElse(false) => {
        Success(Map(name -> input))
      }
      case (ObjectPattern(oPattern), _) if (oPattern == input) => {
        Success(Map.empty)
      }
      case (CasePattern(constructorP, inputP), CaseInstance(constructor, input, _)) if (constructorP == constructor) => {
        attemptPatternMatch(inputP, input, env)
      }
      case _ => Failure("Failed Pattern Match")
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