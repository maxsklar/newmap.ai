package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Evaluates an expression that's already been type checked
object Evaluator {
  def apply(
    nObject: NewMapObject,
    env: Environment,
    keepVersioning: Boolean = false // TODO - can we just call getCurrentConstantValue instead of passing this in?
  ): Outcome[NewMapObject, String] = {
    nObject match {
      case CountT | Index(_) | IndexValue(_, _) | IncrementFunc | TypeT | AnyT | IsCommandFunc | IsSimpleFunction | IsVersionedFunc | IdentifierT | IdentifierInstance(_) | ParamId(_) | ParameterObj(_, _)=> {
        Success(nObject)
      }
      case MapT(inputType, outputType, completeness, featureSet) => {
        // Should I evaluate here?
        Success(nObject)
      }
      case mi@MapInstance(values, mapT) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield mi.copy(values = evalValues)
      }
      case ApplyFunction(func, input) => {
        for {
          evalFunc <- this(func, env)
          evalInput <- this(input, env)

          applicationAttempt <- applyFunctionAttempt(evalFunc, evalInput, env)

          result <- applicationAttempt match {
            case AbleToApplyFunction(nObject) => Success(nObject)
            case UnableToApplyDueToUnknownInput => Success(ApplyFunction(func, evalInput))
          }
        } yield result
      }
      case AccessField(struct, field) => {
        for {
          evalStruct <- this(struct, env)
          result <- accessFieldAttempt(evalStruct, field, env)
        } yield result
      }
      // TODO: pass through eval function type
      case StructT(params) => {
        Success(StructT(params))
      }
      // TODO: pass through eval function type
      case CaseT(cases) => {
        Success(CaseT(cases))
      }
      case StructInstance(value: Vector[(NewMapPattern, NewMapObject)], nType) => {
        for {
          evalValue <- evalParameters(value, env)
        } yield StructInstance(evalValue, nType)
      }
      case ci@CaseInstance(constructor: NewMapObject, input: NewMapObject, caseType: CaseT) => {
        for {
          evalInput <- this(input, env)
        } yield CaseInstance(constructor, evalInput, caseType)
      }
      case SubtypeT(func) => {
        // Is this correct?
        Success(SubtypeT(func))
      }
      case VersionedObject(currentState: NewMapObject, commandType: NewMapObject, versionNumber: Long) => {
        // TODO - do we actually need to do any evaluating on these?
        // - in other words, are versioned objects required to be fully evaluated?
        if (keepVersioning) {
          for {
            evalCurrentState <- this(currentState, env)
            evalCommandType <- this(commandType, env)
          } yield VersionedObject(currentState, commandType, versionNumber)
        } else {
          for {
            evalCurrentState <- this(currentState, env)
          } yield evalCurrentState
        }
      }
      case BranchedVersionedObject(vo, base, changeLog) => {
        if (keepVersioning) {
          Success(BranchedVersionedObject(vo, base, changeLog))
        } else {
          for {
            evalVo <- this(vo, env, keepVersioning = false)
          } yield evalVo
        }
      }
    }
  }

  def getDefaultValueOfCommandType(nSubtype: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    for {
      default <- getDefaultValueOfPureCommandType(RetrieveType.getParentType(nSubtype, env), env)

      isMember <- SubtypeUtils.isMemberOfSubtype(default, nSubtype, env)

      _ <- Outcome.failWhen(
        !isMember,
        s"Result default value $default is not in the subtype"
      )
    } yield default
  }

  def getDefaultValueOfPureCommandType(nType: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => Success(Index(0))
      case Index(i) if i > 0 => Success(IndexValue(0, i))
      case TypeT => Failure("Type of Types has no implemented default value (Maybe it should be empty case)")
      case AnyT => Failure("The \"any\" Type has no implemented default value")
      case IdentifierT => Failure("Type of Identifiers has no default value")
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
        Success(MapInstance(Vector.empty, mapT))
      }
      case MapT(_, _, _, _) => Failure("No default map if not CommandOutput")
      case structT@StructT(params) => {
        for {
          parameterList <- TypeChecker.structParamsIntoParameterList(params)
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
    nType: NewMapObject,
    current: NewMapObject
  ): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => Success(Index(1))
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
        for {
          outputCommandT <- getCommandInputOfPureCommandType(outputType, current)
        } yield {
          StructT(
            MapInstance(
              Vector(ObjectPattern(Index(0)) -> inputType, ObjectPattern(Index(1)) -> outputCommandT),
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
    nType: NewMapObject,
    current: NewMapObject,
    command: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => {
        quickApplyFunctionAttempt(IncrementFunc, current, env)
      }
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
        for {
          input <- accessFieldAttempt(command, Index(0), env)
          commandForInput <- accessFieldAttempt(command, Index(1), env)

          currentResultForInput <- quickApplyFunctionAttempt(current, input, env)

          newResultForInput <- updateVersionedO(outputType, currentResultForInput, commandForInput, env)

          mapValues <- current match {
            case MapInstance(values, _) => Success(values)
            case _ => Failure("Couldn't get map values from $current")
          }

          newMapValues = (ObjectPattern(input) -> newResultForInput) +: mapValues.filter(x => x._1 != ObjectPattern(input))
        } yield {
          MapInstance(newMapValues, mapT)
        }
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

  def lookupVersionedObject(
    id: String,
    env: Environment
  ): Outcome[VersionedObject, String] = {
    for {
      versionedObject <- Outcome(env.lookup(id), s"Identifier $id not found!")

      versionedO <- versionedObject match {
        case vo@VersionedObject(currentState, commandType, versionNumber) => Success(vo)
        case _ => Failure(s"Identifier $id does not point to a versioned object. It is actually $versionedObject.")
      }
    } yield versionedO
  }

  def updateVersionedObject(
    id: String,
    command: NewMapObject,
    env: Environment
  ): Outcome[VersionedObject, String] = {
    for {
      versionedO <- lookupVersionedObject(id, env)
      newState <- updateVersionedO(versionedO.commandType, versionedO.currentState, command, env)
    } yield {
      VersionedObject(newState, versionedO.commandType, versionedO.versionNumber + 1)
    }
    
  }

  def getDefaultValueFromStructParams(
    params: Vector[(NewMapPattern, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(NewMapPattern, NewMapObject)], String] = {
    params match {
      case (id, obj) +: restOfParams => {
        for {
          restOfParamsDefault <- getDefaultValueFromStructParams(restOfParams, env)
          paramDefault <- getDefaultValueOfPureCommandType(RetrieveType.getParentType(obj, env), env)
        } yield {
          (id -> paramDefault) +: restOfParamsDefault
        }
      }
      case _ => Success(Vector.empty)
    }
  }

  def evalMapInstanceVals(
    values: Vector[(NewMapPattern, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(NewMapPattern, NewMapObject)], String] = values match {
    case (k, v) +: restOfValues => {
      for {
        evalK <- evalPattern(k, env)

        nps = newParametersFromPattern(evalK)

        newEnv = env.newParams(nps)

        evalV <- this(v, newEnv)
        evalRest <- evalMapInstanceVals(restOfValues, env)
      } yield {
        (evalK -> evalV) +: evalRest
      }
    }
    case _ => Success(Vector.empty)
  }

  def evalPattern(
    pattern: NewMapPattern,
    env: Environment
  ): Outcome[NewMapPattern, String] = pattern match {
    case ObjectPattern(nObject) => {
      for {
        result <- this(nObject, env)
      } yield ObjectPattern(result)
    }
    case TypePattern(name, nType) => {
      for {
        result <- this(nType, env)
      } yield TypePattern(name, result)
    }
    case StructPattern(params) => {
      for {
        eParams <- evalPatterns(params, env)
      } yield StructPattern(eParams)
    }
  }

  def evalPatterns(
    patterns: Vector[NewMapPattern],
    env: Environment
  ): Outcome[Vector[NewMapPattern], String] = patterns match {
    case pattern +: otherPatterns => {
      for {
        ePattern <- evalPattern(pattern, env)
        eOtherPatterns <- evalPatterns(otherPatterns, env)
      } yield (ePattern +: eOtherPatterns)
    }
    case _ => Success(Vector.empty)
  }

  def evalParameters(
    params: Vector[(NewMapPattern, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(NewMapPattern, NewMapObject)], String] = params match {
    case (k, v) +: restOfValues => {
      for {
        evalV <- this(v, env)

        // TODO: investigate whether we should do something like this
        //newEnv = env.newCommand(FullEnvironmentCommand(k, evalV))

        evalRest <- evalParameters(restOfValues, env)
      } yield {
        (k -> evalV) +: evalRest
      }
    }
    case _ => Success(Vector.empty)
  }

  def accessFieldAttempt(
    struct: NewMapObject, // TODO - this should be a subtype
    field: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    struct match {
      case StructInstance(value: Vector[(NewMapPattern, NewMapObject)], structT) => {
        attemptPatternMatchInOrder(value, field, env)

      }
      case caseT@CaseT(cases) => {
        for {
          // TODO: do we need to do this check if the input is type checked?
          result <- quickApplyFunctionAttempt(cases, field, env)
        } yield {
          val caseInputType = RetrieveType.retrieveInputTypeFromFunction(cases, env)

          MapInstance(
            Vector(
              TypePattern("input", result) -> CaseInstance(field, ParamId("input"), caseT)
            ),
            MapT(result, caseT, RequireCompleteness, SimpleFunction)
          )
        }
      }
      case _ => Failure(s"Unable to access fields from $struct because it is not a struct")
    }
  }

  // This is for an ordered struct.. definitely try to unify this
  def accessFieldsAsList(struct: NewMapObject, env: Environment): Outcome[Vector[NewMapObject], String] = {
    struct match {
      // Should we ensure here that the struct type is correct (I believe it needs to be an ordered BasicMap)
      case StructInstance(values, _) => {
        Success(values.map(_._2))
      }
      case _ => Failure(s"Unable to access fields as a list from $struct")
    }
  }

  sealed abstract class ApplyFunctionAttemptResult
  case class AbleToApplyFunction(nObject: NewMapObject) extends ApplyFunctionAttemptResult
  case object UnableToApplyDueToUnknownInput extends ApplyFunctionAttemptResult

  // Assume that both the function and the input have been evaluated
  // TODO: If there a way to guarantee that this will return something?
  def applyFunctionAttempt(
    func: NewMapObject,
    input: NewMapObject,
    env: Environment
  ): Outcome[ApplyFunctionAttemptResult, String] = {
    // TODO - is there a way to know if the input has already been evaluated as much as possible
    //Evaluate(input, env)
    
    (func, input) match {
      case (_, ParamId(s)) => Success(UnableToApplyDueToUnknownInput)
      case (MapInstance(values, _), key) => {
        for {
          evaluatedKey <- this(key, env)

          keyMatchResult <- key match {
            case ParamId(s) => Success(UnableToApplyDueToUnknownInput)
            case _ => {
              attemptPatternMatchInOrder(values, evaluatedKey, env) match {
                case Success(result) => Success(AbleToApplyFunction(result))
                case Failure(_) => {
                  // Because this is already type checked, we can infer that MapCompleteness == CommandOutput
                  // - If it had equaled "MapCompleteness", then we shouldn't be in a situation with no match
                  // TODO - instead of calling "RetrieveType" on the full object, we should look at the output type of Func,
                  //  and get the default from that
                  val nType = RetrieveType.retrieveOutputTypeFromFunction(func, env)
                  
                  for {
                    initValue <- getDefaultValueOfPureCommandType(RetrieveType.getParentType(nType, env), env)
                  } yield {
                    AbleToApplyFunction(initValue)
                  }
                }
              }
            }
          }
        } yield keyMatchResult
      }
      case (IsCommandFunc, nObject) => {
        val isCommand: Boolean = getDefaultValueOfCommandType(nObject, env).isSuccess

        Success(AbleToApplyFunction(Index(if (isCommand) 1 else 0)))
      }
      case (IsVersionedFunc, nObject) => Success(AbleToApplyFunction(
        nObject match {
          case VersionedObject(_, _, _) => Index(1)
          case BranchedVersionedObject(_, _, _) => Index(1)
          case _ => Index(0)
        }
      ))
      case (IsSimpleFunction, nObject) => {
        nObject match {
          case MapInstance(_, MapT(_, _, CommandOutput, features)) => {
            if (features == SimpleFunction || features == BasicMap) {
              Success(AbleToApplyFunction(Index(1)))
            } else {
              Success(AbleToApplyFunction(Index(0)))
            }
          }
          case _ => Success(AbleToApplyFunction(Index(0)))
        }
      }
      case (IncrementFunc, Index(i)) => Success(AbleToApplyFunction(Index(i + 1)))
      case (AccessField(caseT@CaseT(_), field), _) => {
        Success(AbleToApplyFunction(CaseInstance(field, input, caseT)))
      }
      case _ => {
        Failure(s"Not implemented: apply function\nCallable: $func\nInput: $input")
      }
    }
  }

  // Make sure that nObject has been fully evaluated!
  def quickApplyFunctionAttempt(
    nFunction: NewMapObject,
    nObject: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    for {
      attempt <- applyFunctionAttempt(nFunction, nObject, env)

      result <- attempt match {
        case AbleToApplyFunction(result: NewMapObject) => Success(result)
        case _ => Failure(s"Unable to apply $nObject to function $nFunction ---- $attempt")
      }
    } yield result
  }

  def attemptPatternMatchInOrder(
    remainingPatterns: Vector[(NewMapPattern, NewMapObject)],
    input: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    remainingPatterns match {
      case (pattern, answer) +: addlPatterns => {
        attemptPatternMatch(pattern, input, env) match {
          case Success(paramsToSubsitute) => {
            this(MakeSubstitution(answer, paramsToSubsitute, env), env)
          }
          case Failure(_) => attemptPatternMatchInOrder(addlPatterns, input, env)
        }
      }
      case _ => Failure(s"Unable to pattern match $input, The type checker should have caught this so there may be an error in there")
    }
  }

  def attemptPatternMatch(
    pattern: NewMapPattern,
    input: NewMapObject,
    env: Environment
  ): Outcome[Map[String, NewMapObject], String] = {
    // TODO: IMPORTANT
    // We must be able to dea with using the same variable in a pattern, like StructPattern(x, x) to
    //  denote that these are the same

    // TODO: Fill in the gaps on pattern matching
    // - Case Match needs to be done
    (pattern, input) match {
      case (StructPattern(params), StructInstance(paramValues, _)) => {
        for {
          inputs <- accessFieldsAsList(input, env)
          result <- patternMatchOnStruct(params, inputs, env)
        } yield {
          result
        }
      }
      case (TypePattern(name, nType), _) if SubtypeUtils.isMemberOfSubtype(input, nType, env).toOption.getOrElse(false) => {
        Success(Map(name -> input))
      }
      case (ObjectPattern(oPattern), _) if (oPattern == input) => {
        Success(Map.empty)
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

  // TODO - copy of above, basically, in order to replace it eventually
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
  }

  // This function removes versioning and returns a constant value - the current value
  def getCurrentConstantValue(nObject: NewMapObject): NewMapObject = {
    nObject match {
      case CountT | Index(_) | IndexValue(_, _) | IncrementFunc | TypeT | AnyT | IsCommandFunc | IsSimpleFunction | IsVersionedFunc | IdentifierT | IdentifierInstance(_) | ParamId(_) | ParameterObj(_, _)=> {
        nObject
      }
      case MapT(inputType, outputType, completeness, featureSet) => {
        MapT(getCurrentConstantValue(inputType), getCurrentConstantValue(outputType), completeness, featureSet)
      }
      case mi@MapInstance(values, MapT(inputType, outputType, completeness, featureSet)) => {
        MapInstance(
          constifyMapInstanceVals(values),
          MapT(getCurrentConstantValue(inputType), getCurrentConstantValue(outputType), completeness, featureSet)
        )
      }
      case ApplyFunction(func, input) => {
        ApplyFunction(getCurrentConstantValue(func), getCurrentConstantValue(input))
      }
      case AccessField(struct, field) => {
        AccessField(getCurrentConstantValue(struct), getCurrentConstantValue(field))
      }
      case StructT(params) => {
        StructT(getCurrentConstantValue(params))
      }
      case CaseT(cases) => {
        CaseT(getCurrentConstantValue(cases))
      }
      case StructInstance(value: Vector[(NewMapPattern, NewMapObject)], StructT(params)) => {
        StructInstance(
          constifyMapInstanceVals(value),
          StructT(getCurrentConstantValue(params))
        )
      }
      case ci@CaseInstance(constructor: NewMapObject, input: NewMapObject, CaseT(cases)) => {
        CaseInstance(
          getCurrentConstantValue(constructor),
          getCurrentConstantValue(input),
          CaseT(getCurrentConstantValue(cases))
        )
      }
      case SubtypeT(func) => {
        SubtypeT(getCurrentConstantValue(func))
      }
      case VersionedObject(currentState: NewMapObject, commandType: NewMapObject, versionNumber: Long) => {
        getCurrentConstantValue(currentState)
      }
      case BranchedVersionedObject(vo, base, changeLog) => {
        getCurrentConstantValue(vo)
      }
    }
  }


  def constifyMapInstanceVals(
    values: Vector[(NewMapPattern, NewMapObject)]
  ): Vector[(NewMapPattern, NewMapObject)] = {
    for {
      value <- values
    } yield {
      constifyPattern(value._1) -> getCurrentConstantValue(value._2)
    }
  }

  def constifyPattern(pattern: NewMapPattern): NewMapPattern = {
    pattern match {
      case ObjectPattern(obj) => ObjectPattern(getCurrentConstantValue(obj))
      case TypePattern(name, nType) => TypePattern(name, getCurrentConstantValue(nType))
      case StructPattern(params) => StructPattern(params.map(constifyPattern(_)))
    }
  }
}