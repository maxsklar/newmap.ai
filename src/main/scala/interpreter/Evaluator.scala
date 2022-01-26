package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Evaluates an expression that's already been type checked
object Evaluator {
  def apply(
    nObject: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    nObject match {
      case CountT | Index(_) | RangeFunc(_) | TypeT(_) | IsCommandFunc(_) | IdentifierT | IdentifierInstance(_) | ParameterObj(_, _) | ParameterFunc(_, _, _) | SubstitutableT(_, _) => {
        Success(nObject)
      }
      case MapT(inputType, outputType, completeness, featureSet) => {
        // Should I evaluate here?
        Success(nObject)
      }
      case mi@MapInstance(values: Vector[(NewMapObject, NewMapObject)], mapT) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield mi.copy(values = evalValues)
      }
      case LambdaInstance(lambdaParams, expression) => {
        val newEnv = includeLambdaParams(lambdaParams, env)
        for {
          evalExpression <- this(expression, newEnv)
        } yield {
          LambdaInstance(lambdaParams, evalExpression)
        }
      }
      case ApplyFunction(func, input) => {
        for {
          evalInput <- this(input, env)

          applicationAttempt <- applyFunctionAttempt(func, evalInput, env)

          result <- applicationAttempt match {
            case AbleToApplyFunction(nObject) => Success(nObject)
            case UnableToApplyDueToUnknownInput => Success(ApplyFunction(func, evalInput))
            case NoMatchForInputInFunction => {
              // Because this is already type checked, we can infer that MapCompleteness == CommandOutput
              // - If it had equaled "MapCompleteness", then we shouldn't be in a situation with no match
              val nType = RetrieveType(nObject)
              getDefaultValueOfPureCommandType(RetrieveType.getParentType(nType), env)
            }
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
      case StructInstance(value: Vector[(String, NewMapObject)], nType) => {
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
    }
  }

  def getDefaultValueOfCommandType(nSubtype: NewMapSubtype, env: Environment): Outcome[NewMapObject, String] = {
    for {
      default <- getDefaultValueOfPureCommandType(RetrieveType.getParentType(nSubtype), env)

      isMember <- isMemberOfSubtype(default, nSubtype, env)

      _ <- Outcome.failWhen(
        !isMember,
        s"Result default value $default is not in the subtype"
      )
    } yield default
  }

  def isMemberOfSubtype(
    nObject: NewMapObject,
    nSubtype: NewMapSubtype,
    env: Environment
  ): Outcome[Boolean, String] = {
    nSubtype match {
      case SubtypeT(isMember) => {
        for {
          result <- quickApplyFunctionAttempt(isMember, nObject, env)
          defaultValueOrResultType <- getDefaultValueOfPureCommandType(RetrieveType(result), env)
        } yield (result != defaultValueOrResultType)
      }
      case nType: NewMapType => Success(true)
    }
  }

  def allMembersOfSubtype(
    nObjects: Vector[NewMapObject],
    nSubtype: NewMapSubtype,
    env: Environment
  ): Outcome[Boolean, String] = {
    nObjects match {
      case nObject +: restOfObjects => {
        for {
          isIt <- isMemberOfSubtype(nObject, nSubtype, env)
          _ = println(s"isIt? $isIt -- $nObject -- $nSubtype")
          result <- if (isIt) allMembersOfSubtype(restOfObjects, nSubtype, env) else Success(false)
        } yield result
      }
      case _ => Success(true)
    }
  }

  def getDefaultValueOfPureCommandType(nType: NewMapType, env: Environment): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => Success(Index(0))
      case TypeT(_) => Failure("Type of Types has no implemented default value (Maybe it should be empty case)")
      case IdentifierT => Failure("Type of Identifiers has no default value")
      case mapT@MapT(inputType, outputType, CommandOutput, featureSet) => {
        Success(MapInstance(Vector.empty, mapT))
      }
      case MapT(_, _, _, _) => Failure("No default map if not CommandOutput")
      case structT@StructT(params) => {
        for {
          parameterList <- TypeChecker.structParamsIntoParameterList(params, env)
          paramsT <- TypeChecker.convertParamsObjectToType(parameterList, env)
          defaultValue <- getDefaultValueFromStructParams(paramsT, env)
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
      case SubstitutableT(s, _) => Failure("No default case for subsitutableT " + s)
    }
  }

  def getDefaultValueFromStructParams(
    params: Vector[(String, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapObject)], String] = {
    params match {
      case (s, obj) +: restOfParams => {
        for {
          restOfParamsDefault <- getDefaultValueFromStructParams(restOfParams, env)
          paramType <- Evaluator.convertObjectToType(obj, env)
          paramDefault <- getDefaultValueOfPureCommandType(RetrieveType.getParentType(paramType), env)
        } yield {
          (s -> paramDefault) +: restOfParamsDefault
        }
      }
      case _ => Success(Vector.empty)
    }
  }

  def evalMapInstanceVals(
    values: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = values match {
    case (k, v) +: restOfValues => {
      for {
        evalK <- this(k, env)
        evalV <- this(v, env)

        // TODO: I'm not sure if this is the best place to be altering the Environment
        // Could the env be altered here when it shouldn't be??
        newEnv = (extractIdentifier(evalK), convertObjectToType(evalV, env)) match {
          case (Some(s), Success(t)) => env.newParam(s, t)
          case _ => env
        }

        evalRest <- evalMapInstanceVals(restOfValues, newEnv)
      } yield {
        (evalK -> evalV) +: evalRest
      }
    }
    case _ => Success(Vector.empty)
  }

  def extractIdentifier(nObject: NewMapObject): Option[String] = {
    nObject match {
      case IdentifierInstance(s) => Some(s)
      case _ => None 
    }
  }

  def evalParameters(
    params: Vector[(String, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapObject)], String] = params match {
    case (k, v) +: restOfValues => {
      for {
        evalV <- this(v, env)

        // V Should be a type
        //evalVType <- Evaluator.convertObjectToType(evalV, env)

        // TODO: maybe we can get rid of this entirely!
        //newEnv = env.newCommand(FullEnvironmentCommand(k, evalVType))

        //*********** PROBLEM
        //evalRest <- evalParameters(restOfValues, newEnv)
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
      case StructInstance(value: Vector[(String, NewMapObject)], structT) => {
        // TODO - formalize these - search for everywhere in the code we search a map, and put it in its own function
        // Also edit when struct instance values are Vector[(NewMapObject, NewMapObject)]
        val resultOption = value.find(x => IdentifierInstance(x._1) == field).map(_._2)

        resultOption match {
          case Some(result) => Success(result)
          case None => Failure(s"Unable to access field $field from struct $struct")
        }
      }
      case _ => Failure(s"Unable to access fields from $struct because it is not a struct")
    }
  }

  sealed abstract class ApplyFunctionAttemptResult
  case class AbleToApplyFunction(nObject: NewMapObject) extends ApplyFunctionAttemptResult
  case object UnableToApplyDueToUnknownInput extends ApplyFunctionAttemptResult
  case object NoMatchForInputInFunction extends ApplyFunctionAttemptResult

  // Assume that both the function and the input have been evaluated
  // TODO: If there a way to guarantee that this will return something?
  def applyFunctionAttempt(
    func: NewMapFunction,
    input: NewMapObject,
    env: Environment
  ): Outcome[ApplyFunctionAttemptResult, String] = {
    (func, input) match {
      case (LambdaInstance(IdentifierParam(id, typeOfParam), expression), param) => {
        for {
          nType <- convertObjectToType(typeOfParam, env)
          newEnv = env.newCommand(Environment.eCommand(id, param))
          substitutedExpression = makeRelevantSubstitutions(expression, newEnv)
          result <- this(substitutedExpression, env)
        } yield AbleToApplyFunction(result)
      }
      case (LambdaInstance(StructParams(params), expression), StructInstance(paramValues, _)) => {
        for {
          newEnv <- updateEnvironmentWithParamValues(params, paramValues, env)
          substitutedExpression = makeRelevantSubstitutions(expression, newEnv)
          result <- this(substitutedExpression, env)
        } yield AbleToApplyFunction(result)
      }
      //TODO(2022): Can this be removed?
      /*case (LambdaInstance(StructParams(params), expression), firstParamValue) => {
        // This is the case where the function accepts a Struct as input (multiple parameters),
        // And we are only passing in the first parameter to the function
        for {
          // TODO: there should be an enforcement of at-least-one-param-rule in the param object
          firstParam <- Outcome(params.headOption, "Tried to pass a parameter to a function that takes no parameters")
          newEnv <- updateEnvironmentWithParamValues(Vector(firstParam), Vector(firstParam._1 -> firstParamValue), env)
          substitutedExpression = makeRelevantSubstitutions(expression, newEnv)

          paramErasedExpression = if (params.length == 1) {
            substitutedExpression
          } else {
            val newParams = params.drop(1).map(param => {
              param._1 -> makeRelevantSubstitutions(param._2, newEnv)
            })

            LambdaInstance(StructParams(newParams), substitutedExpression)
          }

          result <- this(NewMapObjectWithType.untyped(paramErasedExpression), env)
        } yield AbleToApplyFunction(result)
      }*/
      case (MapInstance(values, _), key) => {
        for {
          evaluatedKey <- this(key, env)
        } yield {
          key match {
            case ParameterObj(s, _) => UnableToApplyDueToUnknownInput
            case _ => {
              attemptPatternMatchInOrder(values, evaluatedKey, env) match {
                case Success(result) => AbleToApplyFunction(result)
                case Failure(_) => NoMatchForInputInFunction
              }
            }
          }
        }
      }
      case (ParameterFunc(id, _, _), input) => {
        // TODO - in this case the function is unknown, not the input.. so the variable name is technically wrong
        Success(UnableToApplyDueToUnknownInput)
      }
      case (RangeFunc(i), Index(j)) => {
        val ix = if (j < i) 1 else 0
        Success(AbleToApplyFunction(Index(ix)))
      }
      case (IsCommandFunc(i), nObject) => {
        for {
          nType <- Evaluator.convertObjectToType(nObject, env)
          _ <- Outcome.failWhen(RetrieveType(nType) != TypeT(i), s"IsCommandFunc on the wrong level $i for $nType")
        } yield {
          val isCommand: Boolean = getDefaultValueOfCommandType(nType, env).isSuccess
          AbleToApplyFunction(Index(if (isCommand) 1 else 0))
        }
      }
      case _ => {
        Failure("Not implemented: apply function\nCallable: " + func + "\nInput:" + input)
      }
    }
  }

  def quickApplyFunctionAttempt(
    nFunction: NewMapFunction,
    nObject: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    for {
      attempt <- applyFunctionAttempt(nFunction, nObject, env)

      result <- attempt match {
        case AbleToApplyFunction(result: NewMapObject) => Success(result)
        case _ => Failure(s"Unable to apply $nObject to function $nFunction")
      }
    } yield result
  }

  def attemptPatternMatchInOrder(
    remainingPatterns: Vector[(NewMapObject, NewMapObject)],
    input: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    remainingPatterns match {
      case (pattern, answer) +: addlPatterns => {
        val newEnvIfMatched = attemptPatternMatch(pattern, input, env)
        newEnvIfMatched match {
          case Some(newEnv) => Success(makeRelevantSubstitutions(answer, newEnv))
          case None => attemptPatternMatchInOrder(addlPatterns, input, env)
        }
      }
      case _ => Failure(s"Unable to pattern match $input, The type checker should have caught this so there may be an error in there")
    }
  }

  def attemptPatternMatch(
    pattern: NewMapObject,
    input: NewMapObject,
    env: Environment
  ): Option[Environment] = pattern match {
    // TODO: this is too basic of a pattern match!
    // - Also, do we have control over the order of patterns?
    case ParameterObj(param, _) => {
      Some(env.newCommand(FullEnvironmentCommand(param, input)))
    }
    case _ if (pattern == input) => Some(env)
    case _ => None
  }

  def updateEnvironmentWithParamValues(
    paramTypes: Vector[(String, NewMapObject)],
    paramValues: Vector[(String, NewMapObject)],
    env: Environment
  ): Outcome[Environment, String] = {
    (paramTypes, paramValues) match {
      case ((firstParamType +: addlParamTypes), (firstParamValue +: addlParamValues)) => {
        for {
          _ <- Outcome.failWhen(
            firstParamType._1 != firstParamValue._1,
            "Params don't agree: " + firstParamType._1 + " vs " + firstParamValue._1
          )

          envCommand = Environment.eCommand(
            firstParamType._1,
            firstParamValue._2
          )
          newEnv = env.newCommand(envCommand)

          result <- updateEnvironmentWithParamValues(addlParamTypes, addlParamValues, newEnv)
        } yield result
      }
      // TODO - what if one is longer than the other
      case _ => Success(env)
    }
  }

  // TODO(2022): Separate out types and objects again, so that we don't have to deal with all the types in here.
  def makeRelevantSubstitutions(
    expression: NewMapObject,
    env: Environment
  ): NewMapObject = {
    expression match {
      case nType: NewMapSubtype => makeRelevantSubstitutionsOfType(nType, env)
      case nFunction: NewMapFunction => makeRelevantSubstitutionsOfFunction(nFunction, env)
      case Index(_) | IdentifierInstance(_) => expression
      case ApplyFunction(func, input) => {
        ApplyFunction(
          makeRelevantSubstitutionsOfFunction(func, env),
          makeRelevantSubstitutions(input, env)
        )
      }
      case AccessField(struct, field) => {
        AccessField(
          makeRelevantSubstitutions(struct, env),
          makeRelevantSubstitutions(field, env)
        )
      }
      case ParameterObj(name, _) => {
        env.lookup(name) match {
          case Some(obj) => obj
          case None => expression
        }
      }
      case StructInstance(value, nType) => {
        StructInstance(
          value.map(x => (x._1 -> makeRelevantSubstitutions(x._2, env))),
          nType
        )
      }
      case CaseInstance(constructor, value, caseT) => {
        CaseInstance(constructor, makeRelevantSubstitutions(value, env), caseT)      
      }
    }
  }

  def makeRelevantSubstitutionsOfFunction(
    expression: NewMapFunction,
    env: Environment
  ): NewMapFunction = expression match {
    case IsCommandFunc(_) | RangeFunc(_) => expression
    case MapInstance(values, mapT) => {
      val newValues = for {
        (k, v) <- values
      } yield (makeRelevantSubstitutions(k, env) -> makeRelevantSubstitutions(v, env))

      MapInstance(newValues, makeRelevantSubstitutionsOfMapT(mapT, env))
    }
    case LambdaInstance(params, expression) => {
      val newEnv = includeLambdaParams(params, env)
      val newExpression = makeRelevantSubstitutions(expression, newEnv)
      LambdaInstance(params, newExpression)
    }
    case ParameterFunc(name, _, _) => {
      val nObjectOpt = env.lookup(name)
      nObjectOpt match {
        case None => expression
        case Some(nObject) => {
          castToNewMapFunction(nObject) match {
            case Success(result) => result
            case Failure(s) => {
              // TODO - this shouldn't be possible.. can we prevent this line from being needed?
              throw new Exception(s)
            }
          }
        }
      }
    }
  }

  def castToNewMapFunction(nObject: NewMapObject): Outcome[NewMapFunction, String] = {
    nObject match {
      case nFunction: NewMapFunction => Success(nFunction)
      case ParameterObj(name, MapT(inputT, outputT, _, _)) => Success(ParameterFunc(name, inputT, outputT))
      case _ => Failure(s"Couldn't cast object to function: $nObject")
    }
  }

  def makeRelevantSubstitutionsOfMapT(mapT: MapT, env: Environment): MapT = {
    MapT(
      makeRelevantSubstitutionsOfType(mapT.inputType, env),
      makeRelevantSubstitutionsOfType(mapT.outputType, env),
      mapT.completeness,
      mapT.featureSet
    )
  }

  def makeRelevantSubstitutionsOfType(
    expression: NewMapSubtype,
    env: Environment
  ): NewMapSubtype = {
    expression match {
      case CountT | TypeT(_) | IdentifierT => expression
      case MapT(inputType, outputType, completeness, featureSet) => {
        MapT(
          makeRelevantSubstitutionsOfType(inputType, env),
          makeRelevantSubstitutionsOfType(outputType, env),
          completeness,
          featureSet
        )
      }
      case StructT(values) => {
        StructT(makeRelevantSubstitutionsOfFunction(values, env))
      }
      case CaseT(cases) => {
        CaseT(makeRelevantSubstitutionsOfFunction(cases, env))
      }
      case SubstitutableT(s, _) => {
        env.lookup(s) match {
          case Some(obj) => {
            // TODO(2022): This is unsafe! Separate out types in the environment and then it'll be safe again
            Evaluator.convertObjectToType(obj, env).toOption match {
              case Some(t) => t
              case None => throw new Exception(s"Unable to cast to type: $obj")
            }
          }
          case None => expression
        }
      }
      case SubtypeT(isMember) => {
        SubtypeT(makeRelevantSubstitutionsOfFunction(isMember, env))
      }
    }
  }

  def includeLambdaParams(
    lambdaParams: LambdaParamStrategy,
    env: Environment
  ): Environment = {
    lambdaParams match {
      case StructParams(params) => {
        includeParams(params, env)
      }
      case IdentifierParam(param, typeAsObj) => includeParams(Vector(param -> typeAsObj), env)
    }
  }

  def includeParams(
    params: Vector[(String, NewMapObject)],
    env: Environment
  ): Environment = {
    params match {
      case (paramName, paramObj) +: addlParams => {
        // TODO: fix unsafe object to type conversion
        // - This happens when we merge the object and type representations
        val nTypeOpt = convertObjectToType(paramObj, env).toOption

        val newEnv = nTypeOpt match {
          case None => env //TODO
          case Some(nType) => env.newParam(paramName, nType)
        }
        
        includeParams(addlParams, newEnv)
      }
      case _ => env
    }
  }

  // Converts a New Map Object (that is convertible to type Type) into the corresponding NewMapSubtype object
  // TODO(2022): Move this to its own file (or possibly remove entirely!)
  def convertObjectToType(
    objectFound: NewMapObject,
    env: Environment
  ): Outcome[NewMapSubtype, String] = {
    objectFound match {
      case Index(i) => Failure("Can't convert index to type") // Try to convert it to a subtype of Count?
      case CountT => Success(CountT)
      case TypeT(i) => Success(TypeT(i))
      case IsCommandFunc(_) => Failure("Can't convert IsCommandFunc to type")
      case IdentifierT => Success(IdentifierT)
      case SubstitutableT(s, nType) => Success(SubstitutableT(s, nType))
      case MapT(inputType, outputType, completeness, featureSet) => {
        Success(MapT(inputType, outputType, completeness, featureSet))
      }
      case mi@MapInstance(values, _) => {
        // TODO - require an explicit conversion here? Maybe this should be left to struct type
        for {
          // TODO - is the convertMapInstanceStructToParams appropriate for cases?
          newParams <- convertMapInstanceStructToParams(values, env)
        } yield {
          // Should we be doing this?
          // PROBLEM!!!!!!! - sometimes it should be case
          StructT(mi)
        }
      }
      case ParameterObj(name, nType) => {
        // TODO: Should this even be allowed?
        // (maybe write a test to prove that it isn't a then remove)
        Success(SubstitutableT(name, nType))
      }
      case ApplyFunction(func, input) => {
        Failure(s"Expression $func($input) is not a type - perhaps if it is evaluated")
      }
      case StructT(params) => Success(StructT(params))
      case CaseT(cases) => Success(CaseT(cases))
      case IdentifierInstance(name) => {
        Failure("Identifier " + name + " is not connected to a type.")
      }
      case SubtypeT(isMember) => Success(SubtypeT(isMember))
      case _ => {
        Failure("Couldn't convert into type: " + objectFound + " -- could be unimplemented")
      }
    }
  }

  def convertMapInstanceStructToParams(
    values: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapSubtype)], String] = {
    values match {
      case (key, value) +: restOfValues => key match {
        case IdentifierInstance(s) => {
          for {
            valueType <- convertObjectToType(value, env)
            restOfParams <- convertMapInstanceStructToParams(restOfValues, env.newParam(s, valueType))
          } yield {
            (s -> valueType) +: restOfParams
          }
        }
        // TODO - what if the key substitutes to an identifier?? Better Logic on that
        case _ => Failure("Key must be identifier: " + key)
      }
      case _ => {
        Success(Vector.empty)
      }
    }
  }
}