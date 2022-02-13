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
      case CountT | Index(_) | RangeFunc(_) | IncrementFunc | TypeT | AnyT | IsCommandFunc | IdentifierT | IdentifierInstance(_) | IdentifierPattern(_, _) | ParameterObj(_, _) => {
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
              // TODO - instead of calling "RetrieveType" on the full object, we should look at the output type of Func,
              //  and get the default from that
              val nType = RetrieveType(nObject)
              getDefaultValueOfPureCommandType(RetrieveType.getParentType(nType))
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
      case StructInstance(value: Vector[(NewMapObject, NewMapObject)], nType) => {
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

  def getDefaultValueOfCommandType(nSubtype: NewMapObject, env: Environment): Outcome[NewMapObject, String] = {
    for {
      default <- getDefaultValueOfPureCommandType(RetrieveType.getParentType(nSubtype))

      isMember <- SubtypeUtils.isMemberOfSubtype(default, nSubtype, env)

      _ <- Outcome.failWhen(
        !isMember,
        s"Result default value $default is not in the subtype"
      )
    } yield default
  }

  def getDefaultValueOfPureCommandType(nType: NewMapObject): Outcome[NewMapObject, String] = {
    nType match {
      case CountT => Success(Index(0))
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
          defaultValue <- getDefaultValueFromStructParams(parameterList)
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

  def getDefaultValueFromStructParams(
    params: Vector[(NewMapObject, NewMapObject)]
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    params match {
      case (id, obj) +: restOfParams => {
        for {
          restOfParamsDefault <- getDefaultValueFromStructParams(restOfParams)
          paramDefault <- getDefaultValueOfPureCommandType(RetrieveType.getParentType(obj))
        } yield {
          (id -> paramDefault) +: restOfParamsDefault
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
        newEnv = extractIdentifier(evalK) match {
          case Some(s) => env.newParam(s, evalV)
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
    params: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = params match {
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
      case StructInstance(value: Vector[(NewMapObject, NewMapObject)], structT) => {
        // TODO - formalize these - search for everywhere in the code we search a map, and put it in its own function
        // Also edit when struct instance values are Vector[(NewMapObject, NewMapObject)]
        val resultOption = value.find(x => x._1 == field).map(_._2)

        resultOption match {
          case Some(result) => Success(result)
          case None => Failure(s"Unable to access field $field from struct $struct")
        }
      }
      case caseT@CaseT(cases) => {
        for {
          // TODO: do we need to do this check if the input is type checked?
          result <- quickApplyFunctionAttempt(cases, field, env)
        } yield {
          LambdaInstance(
            IdentifierParam("input", RetrieveType.retrieveInputTypeFromFunction(cases)),
            CaseInstance(field, ParameterObj("input", RetrieveType.retrieveInputTypeFromFunction(cases)), caseT)
          )
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
    func: NewMapObject,
    input: NewMapObject,
    env: Environment
  ): Outcome[ApplyFunctionAttemptResult, String] = {
    // TODO - is there a way to know if the input has already been evaluated as much as possible
    //Evaluate(input, env)
    
    (func, input) match {
      case (_, ParameterObj(s, _)) => Success(UnableToApplyDueToUnknownInput)
      case (LambdaInstance(IdentifierParam(id, nType), expression), param) => {
        val newEnv = env.newCommand(Environment.eCommand(id, param))
        val substitutedExpression = MakeSubstitution(expression, newEnv)

        for {
          result <- this(substitutedExpression, env)
        } yield AbleToApplyFunction(result)
      }
      case (LambdaInstance(StructParams(params), expression), StructInstance(paramValues, _)) => {
        for {
          newEnv <- updateEnvironmentWithParamValues(params, paramValues, env)
          substitutedExpression = MakeSubstitution(expression, newEnv)
          result <- this(substitutedExpression, env)
        } yield AbleToApplyFunction(result)
      }
      case (LambdaInstance(StructParams(params), expression), param) if (params.length == 1) => {
        val (id, nType) = params.head
        val newEnv = env.newCommand(FullEnvironmentCommand(id, param))
        val substitutedExpression = MakeSubstitution(expression, newEnv)
        for {
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
          substitutedExpression = MakeSubstitution(expression, newEnv)

          paramErasedExpression = if (params.length == 1) {
            substitutedExpression
          } else {
            val newParams = params.drop(1).map(param => {
              param._1 -> MakeSubstitution(param._2, newEnv)
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
      case (RangeFunc(i), Index(j)) => {
        val ix = if (j < i) 1 else 0
        Success(AbleToApplyFunction(Index(ix)))
      }
      case (IsCommandFunc, nObject) => {
        val isCommand: Boolean = getDefaultValueOfCommandType(nObject, env).isSuccess
        Success(AbleToApplyFunction(Index(if (isCommand) 1 else 0)))
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
        val newEnvCommandsIfMatched = attemptPatternMatch(pattern, input, env)
        newEnvCommandsIfMatched match {
          case Success(envCommands) => {
            val newEnv = env.newCommands(envCommands)
            this(MakeSubstitution(answer, newEnv), env)
          }
          case Failure(_) => attemptPatternMatchInOrder(addlPatterns, input, env)
        }
      }
      case _ => Failure(s"Unable to pattern match $input, The type checker should have caught this so there may be an error in there")
    }
  }

  def attemptPatternMatch(
    pattern: NewMapObject,
    input: NewMapObject,
    env: Environment
  ): Outcome[Vector[FullEnvironmentCommand], String] = {
    // TODO: this is too basic of a pattern match! Fill in the gaps
    pattern match {
      
      case IdentifierPattern(param, _) => {
        Success(Vector(FullEnvironmentCommand(IdentifierInstance(param), input)))
      }
      case StructInstance(values, _) => patternMatchOnStruct(values, input, env)
      case _ if (pattern == input) => Success(Vector.empty)
      case _ => Failure("Failed Pattern Match")
    }
  }

  def patternMatchOnStruct(
    structPattern: Vector[(NewMapObject, NewMapObject)],
    input: NewMapObject,
    env: Environment
  ): Outcome[Vector[FullEnvironmentCommand], String] = {
    structPattern match {
      case (firstField, firstPattern) +: restOfPattern => {
        for {
          result <- accessFieldAttempt(input, firstField, env)
          envCommands <- attemptPatternMatch(firstPattern, result, env)

          newEnv = env.newCommands(envCommands)

          restOfEnvCommands <- patternMatchOnStruct(restOfPattern, input, newEnv)
        } yield {
          envCommands ++ restOfEnvCommands
        }
      }
      case _ => Success(Vector.empty) // No patterns to match
    }
  }

  def updateEnvironmentWithParamValues(
    paramTypes: Vector[(NewMapObject, NewMapObject)],
    paramValues: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Outcome[Environment, String] = {
    (paramTypes, paramValues) match {
      case ((firstParamType +: addlParamTypes), (firstParamValue +: addlParamValues)) => {
        for {
          _ <- Outcome.failWhen(
            firstParamType._1 != firstParamValue._1,
            "Params don't agree: " + firstParamType._1 + " vs " + firstParamValue._1
          )

          envCommand = FullEnvironmentCommand(firstParamType._1, firstParamValue._2)
          newEnv = env.newCommand(envCommand)

          result <- updateEnvironmentWithParamValues(addlParamTypes, addlParamValues, newEnv)
        } yield result
      }
      // TODO - what if one is longer than the other
      case _ => Success(env)
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
      case IdentifierParam(param, typeAsObj) => includeParams(Vector(IdentifierInstance(param) -> typeAsObj), env)
    }
  }

  def includeParams(
    params: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Environment = {
    params match {
      case (paramName, nType) +: addlParams => {
        val newEnv = paramName match {
          case IdentifierInstance(i) => env.newCommand(
            FullEnvironmentCommand(paramName, ParameterObj(i, nType))
          )
          case _ => env //TODO
        }
        
        includeParams(addlParams, newEnv)
      }
      case _ => env
    }
  }
}