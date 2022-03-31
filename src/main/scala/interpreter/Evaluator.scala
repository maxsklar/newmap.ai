package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Evaluates an expression that's already been type checked
object Evaluator {
  def apply(
    nObject: NewMapObject,
    env: Environment,
    keepVersioning: Boolean = true // TODO - can we just call getCurrentConstantValue instead of passing this in?
  ): Outcome[NewMapObject, String] = {
    nObject match {
      case CountT | Index(_) | IndexValue(_, _) | IncrementFunc | TypeT | AnyT | IsCommandFunc | IsSimpleFunction | IsVersionedFunc | IdentifierT | IdentifierInstance(_) | ParamId(_) | ParameterObj(_, _)=> {
        Success(nObject)
      }
      case MapT(inputType, outputType, completeness, featureSet) => {
        // Should I evaluate here?
        Success(nObject)
      }
      case SequenceT(nType) => {
        // Should I evaluate here?
        Success(nObject)
      }
      case mi@MapInstance(values, mapT) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield mi.copy(values = evalValues)
      }
      case si@SequenceInstance(values, seqT) => {
        for {
          evalValues <- evalSeqInstanceVals(values, env)
        } yield SequenceInstance(evalValues, seqT)
      }
      case ApplyFunction(func, input) => {
        for {
          evalFunc <- this(func, env)
          evalInput <- this(input, env)

          applicationAttempt <- applyFunctionAttempt(evalFunc, evalInput, env)

          result <- applicationAttempt match {
            case AbleToApplyFunction(nObject) => Success(nObject)
            case UnableToApplyDueToFreeVariables => Success(ApplyFunction(func, evalInput))
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
      case CaseInstance(constructor: NewMapObject, input: NewMapObject, caseType) => {
        for {
          evalInput <- this(input, env)
        } yield CaseInstance(constructor, evalInput, caseType)
      }
      case SubtypeT(func) => {
        // Is this correct?
        Success(SubtypeT(func))
      }
      case vol@VersionedObjectLink(_, _) => {
        // TODO - do we actually need to do any evaluating on these?
        // - in other words, are versioned objects required to be fully evaluated?
        if (keepVersioning) {
          Success(vol)
        } else {
          Success(getCurrentConstantValue(vol, env))
        }
      }
    }
  }

  def getDefaultValueOfCommandType(nType: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => Success(Index(0))
      case seqT@SequenceT(nType) => Success(SequenceInstance(Vector.empty, seqT))
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
      case SequenceT(nType) => Success(nType)
      // This should really be a case type instead of a typeT
      case TypeT => {
        Success(
          StructT(
            MapInstance(
              Vector(ObjectPattern(Index(0)) -> IdentifierT, ObjectPattern(Index(1)) -> TypeT),
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
    RetrieveType(current, env) match {
      case CountT => {
        quickApplyFunctionAttempt(IncrementFunc, current, env)
      }
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
        for {
          input <- accessFieldAttempt(command, Index(0), env)
          commandForInput <- accessFieldAttempt(command, Index(1), env)

          currentResultForInput <- quickApplyFunctionAttempt(current, input, env)

          newResultForInput <- updateVersionedO(currentResultForInput, commandForInput, env)

          mapValues <- current match {
            case MapInstance(values, _) => Success(values)
            case _ => Failure(s"Couldn't get map values from $current")
          }

          newMapValues = (ObjectPattern(input) -> newResultForInput) +: mapValues.filter(x => x._1 != ObjectPattern(input))
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
      case TypeT => {
        current match {
          case CaseT(MapInstance(values, mapT)) => {
            // General map, adding a case
            for {
              newCaseName <- accessFieldAttempt(command, Index(0), env)
              newCaseInputType <- accessFieldAttempt(command, Index(1), env)


              newMapValues = (ObjectPattern(newCaseName) -> newCaseInputType) +: values.filter(x => x._1 != ObjectPattern(newCaseName))
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

      versionedO <- versionedObject match {
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
      UpdateVersionedObjectResponse(latestVersion, versionLink.key.uuid, newState)
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
          paramDefault <- getDefaultValueOfCommandType(RetrieveType.getParentType(obj, env), env)
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

  def evalSeqInstanceVals(
    values: Vector[NewMapObject],
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
    case CasePattern(constructor, input) => {
      for {
        result <- evalPattern(input, env)
      } yield CasePattern(constructor, input)
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

  // Assume that this is type checked so the field should exist in the struct
  def accessFieldAttempt(
    struct: NewMapObject,
    field: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    getCurrentConstantValue(struct, env) match {
      case StructInstance(value: Vector[(NewMapPattern, NewMapObject)], structT) => {
        attemptPatternMatchInOrder(value, field, env)

      }
      case CaseT(cases) => {
        for {
          result <- quickApplyFunctionAttempt(cases, field, env)
        } yield {
          val caseInputType = RetrieveType.retrieveInputTypeFromFunction(cases, env)

          MapInstance(
            Vector(
              TypePattern("input", result) -> CaseInstance(field, ParamId("input"), struct)
            ),
            MapT(result, struct, RequireCompleteness, SimpleFunction)
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
  case object UnableToApplyDueToFreeVariables extends ApplyFunctionAttemptResult

  // Assume that both the function and the input have been evaluated
  def applyFunctionAttempt(
    func: NewMapObject,
    input: NewMapObject,
    env: Environment
  ): Outcome[ApplyFunctionAttemptResult, String] = {
    // TODO - is there a way to know if the input has already been evaluated as much as possible
    //Evaluate(input, env)
    // TODO - can we make sure that we get the Current Constant Value BEFORE this is called?
    val funcC = getCurrentConstantValue(func, env)
    val inputC = getCurrentConstantValue(input, env)
    (funcC, inputC) match {
      case (_, ParamId(s)) => Success(UnableToApplyDueToFreeVariables)
      case (ParamId(s), _) => Success(UnableToApplyDueToFreeVariables)
      case (MapInstance(values, _), key) => {
        for {
          evaluatedKey <- this(key, env)

          keyMatchResult <- key match {
            case ParamId(s) => Success(UnableToApplyDueToFreeVariables)
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
                    initValue <- getDefaultValueOfCommandType(RetrieveType.getParentType(nType, env), env)
                  } yield {
                    AbleToApplyFunction(initValue)
                  }
                }
              }
            }
          }
        } yield keyMatchResult
      }
      // TODO: sequences should have their own indecies rather than checking values.length
      case (SequenceInstance(values, seqT), IndexValue(i, Index(j))) if (j == values.length) => {
        Success(AbleToApplyFunction(values(i.toInt)))
      }
      case (IsCommandFunc, nObject) => {
        val isCommand: Boolean = getDefaultValueOfCommandType(nObject, env).isSuccess

        Success(AbleToApplyFunction(Index(if (isCommand) 1 else 0)))
      }
      case (IsVersionedFunc, nObject) => Success(AbleToApplyFunction(
        nObject match {
          case VersionedObjectLink(_, _) => Index(1)
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
          inputs <- accessFieldsAsList(input, env)
          result <- patternMatchOnStruct(params, inputs, env)
        } yield {
          result
        }
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
    case CasePattern(constructor, input) => {
      newParametersFromPattern(input)
    }
  }

  // This function removes versioning and returns a constant value - the current value
  def getCurrentConstantValue(nObject: NewMapObject, env: Environment): NewMapObject = {
    nObject match {
      case CountT | Index(_) | IndexValue(_, _) | IncrementFunc | TypeT | AnyT | IsCommandFunc | IsSimpleFunction | IsVersionedFunc | IdentifierT | IdentifierInstance(_) | ParamId(_) | ParameterObj(_, _)=> {
        nObject
      }
      case MapT(inputType, outputType, completeness, featureSet) => {
        MapT(getCurrentConstantValue(inputType, env), getCurrentConstantValue(outputType, env), completeness, featureSet)
      }
      case SequenceT(nType) => SequenceT(getCurrentConstantValue(nType, env))
      case MapInstance(values, mapT) => {
        MapInstance(
          constifyMapInstanceVals(values, env),
          getCurrentConstantValue(mapT, env)
        )
      }
      case SequenceInstance(values, seqT) => {
        SequenceInstance(
          values.map(getCurrentConstantValue(_, env)),
          getCurrentConstantValue(seqT, env)
        )
      }
      case ApplyFunction(func, input) => {
        ApplyFunction(getCurrentConstantValue(func, env), getCurrentConstantValue(input, env))
      }
      case AccessField(struct, field) => {
        AccessField(getCurrentConstantValue(struct, env), getCurrentConstantValue(field, env))
      }
      case StructT(params) => {
        StructT(getCurrentConstantValue(params, env))
      }
      case CaseT(cases) => {
        CaseT(getCurrentConstantValue(cases, env))
      }
      case StructInstance(value: Vector[(NewMapPattern, NewMapObject)], structT) => {
        StructInstance(
          constifyMapInstanceVals(value, env),
          getCurrentConstantValue(structT, env)
        )
      }
      case ci@CaseInstance(constructor: NewMapObject, input: NewMapObject, caseT) => {
        CaseInstance(
          getCurrentConstantValue(constructor, env),
          getCurrentConstantValue(input, env),
          getCurrentConstantValue(caseT, env)
        )
      }
      case SubtypeT(func) => {
        SubtypeT(getCurrentConstantValue(func, env))
      }
      case VersionedObjectLink(key, status) => {
        // TODO - make this function an outcome
        this(currentState(key.uuid, env).toOption.get, env).toOption.get
      }
    }
  }

  def constifyMapInstanceVals(
    values: Vector[(NewMapPattern, NewMapObject)],
    env: Environment
  ): Vector[(NewMapPattern, NewMapObject)] = {
    for {
      value <- values
    } yield {
      constifyPattern(value._1, env) -> getCurrentConstantValue(value._2, env)
    }
  }

  def constifyPattern(pattern: NewMapPattern, env: Environment): NewMapPattern = {
    pattern match {
      case ObjectPattern(obj) => ObjectPattern(getCurrentConstantValue(obj, env))
      case TypePattern(name, nType) => TypePattern(name, getCurrentConstantValue(nType, env))
      case StructPattern(params) => StructPattern(params.map(constifyPattern(_, env)))
      case CasePattern(constructor, input) => CasePattern(constructor, constifyPattern(input, env))
    }
  }
}