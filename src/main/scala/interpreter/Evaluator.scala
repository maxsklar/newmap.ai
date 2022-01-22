package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Evaluates an expression that's already been type checked
object Evaluator {
  def apply(
    nObjectWithType: NewMapObjectWithType,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    val nObject = nObjectWithType.nObject
    nObject match {
      case Index(_) | CountT | TypeT | CommandTypeT | IdentifierT | IdentifierInstance(_) | ParameterObj(_) | SubstitutableT(_) => {
        Success(nObject)
      }
      case MapT(inputType, outputType, completeness, featureSet) => {
        val typeOfTheOutputType = {
          if (completeness == CommandOutput) CommandTypeT else TypeT
        }

        for {
          evalInputType <- this(NewMapObjectWithType.withTypeE(inputType, TypeT), env)
          evalOutputType <- this(NewMapObjectWithType.withTypeE(outputType, typeOfTheOutputType), env)
        } yield {
          MapT(evalInputType, evalOutputType, completeness, featureSet)
        }
      }
      case MapInstance(values: Vector[(NewMapObject, NewMapObject)]) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield MapInstance(evalValues)
      }
      case LambdaInstance(lambdaParams, expression) => {
        val newEnv = includeLambdaParams(lambdaParams, env)
        for {
          evalExpression <- this(NewMapObjectWithType.untyped(expression), newEnv)
        } yield {
          LambdaInstance(lambdaParams, evalExpression)
        }
      }
      case ApplyFunction(func, input) => {
        for {
          evalInput <- this(NewMapObjectWithType.untyped(input), env)
          evalFunc <- this(NewMapObjectWithType.untyped(func), env)

          applicationAttempt <- applyFunctionAttempt(evalFunc, evalInput, env)

          result <- applicationAttempt match {
            case AbleToApplyFunction(nObject) => Success(nObject)
            case UnableToApplyDueToUnknownInput => Success(ApplyFunction(evalFunc, evalInput))
            case NoMatchForInputInFunction => {
              // Because this is already type checked, we can infer that MapCompleteness == CommandOutput
              // - If it had equaled "MapCompleteness", then we shouldn't be in a situation with no match
              nObjectWithType.nTypeInfo match {
                case ExplicitlyTyped(nType) => getDefaultValueOfCommandType(nType, env)
                case ImplicitlyTyped(_) => Failure("Could not get default value for implicit type, currently unimplemented")
              }
            }
          }
        } yield result
      }
      case StructT(fieldType, params) => {
        for {
          evalFieldType <- this(NewMapObjectWithType.withTypeE(fieldType, TypeT), env)
          evalParams <- this(
            NewMapObjectWithType.withTypeE(
              params,
              MapT(evalFieldType, TypeT, RequireCompleteness, BasicMap)
            ),
            env
          )
        } yield StructT(evalFieldType, evalParams)
      }
      case CaseT(params) => {
        for {
          evalParams <- evalParameters(params, env)
        } yield CaseT(evalParams)
      }
      case StructInstance(value: Vector[(String, NewMapObject)]) => {
        for {
          evalValue <- evalParameters(value, env)
        } yield StructInstance(evalValue)
      }
      case CaseInstance(constructor: String, input: NewMapObject) => {
        for {
          evalInput <- this(NewMapObjectWithType.untyped(input), env)
        } yield CaseInstance(constructor, evalInput)
      }
      case AppendToSeq(currentSeq, newValue) => {
        for {
          evalCurrentSeq <- this(NewMapObjectWithType.untyped(currentSeq), env)
          evalNewValue <- this(NewMapObjectWithType.untyped(newValue), env)
        } yield {
          evalCurrentSeq match {
            case MapInstance(values) => {
              val keyNums: Vector[Long] = values.map(_._1).flatMap(extractNumber(_))

              val newIndex = if (keyNums.isEmpty) 0 else (keyNums.max + 1)

              MapInstance(values ++ Vector(Index(newIndex) -> evalNewValue))
            }
            case _ => AppendToSeq(evalCurrentSeq, evalNewValue)
          }
        }
      }
      case AppendToMap(currentMap, newValues) => {
        for {
          evalCurrentMap <- this(NewMapObjectWithType.untyped(currentMap), env)
          evalNewValues <- this(NewMapObjectWithType.untyped(newValues), env)
        } yield {
          (evalCurrentMap, evalNewValues) match {
            case (MapInstance(values), MapInstance(newValues)) => {
              val updatedKeys = newValues.map(_._1).toSet
              val removedValues = values.filter(v => !updatedKeys.contains(v._1))

              MapInstance(removedValues ++ newValues)
            }
            case _ => AppendToMap(evalCurrentMap, evalNewValues)
          }
        }
      }
      case Subtype(parentType, func) => {
        for {
          parentT <- Evaluator.convertObjectToType(parentType, env)
          evalFunc <- this(NewMapObjectWithType.withTypeE(func, parentT), env)
        } yield {
          Subtype(parentType, evalFunc)
        }
      }
    }
  }

  def getDefaultValueOfCommandType(nType: NewMapType, env: Environment): Outcome[NewMapObject, String] = {
    nType match {
      case Index(0) => Failure("Zero Type has no default value")
      case Index(i) => Success(Index(0))
      case CountT => Success(Index(0))
      case TypeT => Failure("Type of Types has no implemented default value (Maybe it should be empty case)")
      case CommandTypeT => Failure("Type of Command Types has no implemented default value")
      case IdentifierT => Failure("Type of Identifiers has no default value")
      case MapT(inputType, outputType, CommandOutput, _) => Success(MapInstance(Vector.empty))
      case MapT(_, _, _, _) => Failure("No default map if not CommandOutput")
      case StructT(fieldType, params) => {
        for {
          parameterList <- TypeChecker.structParamsIntoParameterList(params, env)
          paramsT <- TypeChecker.convertParamsObjectToType(parameterList, env)
          defaultValue <- getDefaultValueFromStructParams(paramsT, env)
        } yield {
          StructInstance(defaultValue)
        }
      }
      case CaseT(params) if (params.length == 0) => Failure("Empty Case Types do not have a default value")
      case CaseT(params) => {
        // TODO: Revisit when revisiting Cases
        for {
          t <- Evaluator.convertObjectToType(params.head._2, env)
          initValueInFirstField <- getDefaultValueOfCommandType(t, env)
        } yield {
          CaseInstance(params.head._1, initValueInFirstField)
        }
      }
      case SubstitutableT(s) => Failure("No default case for subsitutableT " + s)
      case Subtype(_, _) => Failure("No default case for a subtype")
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
          paramDefault <- getDefaultValueOfCommandType(paramType, env)
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
        evalK <- this(NewMapObjectWithType.untyped(k), env)
        evalV <- this(NewMapObjectWithType.untyped(v), env)

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

  def extractNumber(nObject: NewMapObject): Option[Long] = {
    nObject match {
      case Index(i) => Some(i)
      case _ => None
    }
  }

  def evalParameters(
    params: Vector[(String, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapObject)], String] = params match {
    case (k, v) +: restOfValues => {
      for {
        evalV <- this(NewMapObjectWithType.untyped(v), env)

        // TODO: we should bring in the type here
        newEnv = env.newCommand(FullEnvironmentCommand(k, NewMapObjectWithType.untyped(evalV)))

        evalRest <- evalParameters(restOfValues, newEnv)
      } yield {
        (k -> evalV) +: evalRest
      }
    }
    case _ => Success(Vector.empty)
  }

  sealed abstract class ApplyFunctionAttemptResult
  case class AbleToApplyFunction(nObject: NewMapObject) extends ApplyFunctionAttemptResult
  case object UnableToApplyDueToUnknownInput extends ApplyFunctionAttemptResult
  case object NoMatchForInputInFunction extends ApplyFunctionAttemptResult

  // Assume that both the function and the input have been evaluated
  def applyFunctionAttempt(
    func: NewMapObject,
    input: NewMapObject,
    env: Environment
  ): Outcome[ApplyFunctionAttemptResult, String] = {
    (func, input) match {
      case (LambdaInstance(IdentifierParam(id, typeOfParam), expression), param) => {
        for {
          nType <- convertObjectToType(typeOfParam, env)
          newEnv = env.newCommand(Environment.eCommand(id, nType, param))
          substitutedExpression = makeRelevantSubstitutions(expression, newEnv)
          result <- this(NewMapObjectWithType.untyped(substitutedExpression), env)
        } yield AbleToApplyFunction(result)
      }
      case (LambdaInstance(InputStackParam(typeOfObj), expression), param) => {
        Failure("We don't currently have an input stack: " + expression)
      }
      case (LambdaInstance(StructParams(params), expression), StructInstance(paramValues)) => {
        for {
          newEnv <- updateEnvironmentWithParamValues(params, paramValues, env)
          substitutedExpression = makeRelevantSubstitutions(expression, newEnv)
          result <- this(NewMapObjectWithType.untyped(substitutedExpression), env)
        } yield AbleToApplyFunction(result)
      }
      case (LambdaInstance(StructParams(params), expression), firstParamValue) => {
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
      }
      case (MapInstance(values), key) => {
        for {
          evaluatedKey <- this(NewMapObjectWithType.untyped(key), env)
        } yield {
          key match {
            case ParameterObj(s) => UnableToApplyDueToUnknownInput
            case _ => {
              attemptPatternMatchInOrder(values, evaluatedKey, env) match {
                case Success(result) => AbleToApplyFunction(result)
                case Failure(_) => NoMatchForInputInFunction
              }
            }
          }
        }
      }
      case (StructInstance(value: Vector[(String, NewMapObject)]), identifier) => {
        val id = makeRelevantSubstitutions(identifier, env)
        Success(
          AbleToApplyFunction(
            value.find(x => IdentifierInstance(x._1) == id).map(_._2).getOrElse(Index(0))
          )
        )
      }
      case (ParameterObj(id), input) => {
        // TODO - in this case the function is unknown, not the input.. so the variable name is technically wrong
        Success(UnableToApplyDueToUnknownInput)
      }
      case _ => {
        Failure("Not implemented: apply function\nCallable: " + func + "\nInput:" + input)
      }
    }
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
    case ParameterObj(param) => {
      Some(env.newCommand(FullEnvironmentCommand(
        param,
        NewMapObjectWithType.untyped(input)
      )))
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
          typeInformation <- convertObjectToType(firstParamType._2, env)

          envCommand = Environment.eCommand(
            firstParamType._1,
            typeInformation,
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

  def makeRelevantSubstitutions(
    expression: NewMapObject,
    env: Environment
  ): NewMapObject = {
    expression match {
      case Index(_) | CountT | TypeT | CommandTypeT | IdentifierT | IdentifierInstance(_) => expression
      case MapT(inputType, outputType, completeness, featureSet) => {
        MapT(
          makeRelevantSubstitutions(inputType, env),
          makeRelevantSubstitutions(outputType, env),
          completeness,
          featureSet
        )
      }
      case MapInstance(values) => {
        val newValues = for {
          (k, v) <- values
        } yield (makeRelevantSubstitutions(k, env) -> makeRelevantSubstitutions(v, env))

        MapInstance(newValues)
      }
      case LambdaInstance(params, expression) => {
        val newEnv = includeLambdaParams(params, env)
        val newExpression = makeRelevantSubstitutions(expression, newEnv)
        LambdaInstance(params, newExpression)
      }
      case ApplyFunction(func, input) => {
        ApplyFunction(
          makeRelevantSubstitutions(func, env),
          makeRelevantSubstitutions(input, env)
        )
      }
      case ParameterObj(name) => {
        env.objectOf(name) match {
          case Some(obj) => obj
          case None => expression
        }
      }
      case StructT(fieldType, values) => {
        StructT(
          makeRelevantSubstitutions(fieldType, env),
          makeRelevantSubstitutions(values, env)
        )
      }
      case CaseT(values) => {
        val substitutedValues = {
          values.map(x => (x._1, makeRelevantSubstitutions(x._2, env)))
        }

        CaseT(substitutedValues)
      }
      case StructInstance(value) => {
        StructInstance(value.map(x => (x._1 -> makeRelevantSubstitutions(x._2, env))))
      }
      case CaseInstance(constructor, value) => {
        CaseInstance(constructor, makeRelevantSubstitutions(value, env))      
      }
      case AppendToSeq(currentSeq, newValue) => {
        AppendToSeq(makeRelevantSubstitutions(currentSeq, env), makeRelevantSubstitutions(newValue, env))
      }
      case AppendToMap(currentMap, newValues) => {
        AppendToMap(makeRelevantSubstitutions(currentMap, env), makeRelevantSubstitutions(newValues, env))
      }
      case SubstitutableT(s) => {
        env.objectOf(s) match {
          case Some(obj) => obj
          case None => expression
        }
      }
      case Subtype(parentType, func) => {
        Subtype(
          makeRelevantSubstitutions(parentType, env),
          makeRelevantSubstitutions(func, env)
        )
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
      case InputStackParam(typeAsObj) => {
        // TODO - add to the input stack
        env
      }
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

  // Converts a New Map Object (that is convertible to type Type) into the corresponding NewMapType object
  // TODO(2022): Move this to its own file (or possibly remove entirely!)
  def convertObjectToType(
    objectFound: NewMapObject,
    env: Environment
  ): Outcome[NewMapType, String] = {
    objectFound match {
      case Index(i) => Success(Index(i))
      case TypeT => Success(TypeT)
      case CommandTypeT => Success(CommandTypeT)
      case CountT => Success(CountT)
      case IdentifierT => Success(IdentifierT)
      case SubstitutableT(s) => Success(SubstitutableT(s))
      case Subtype(parentType, func) => Success(Subtype(parentType, func))
      case MapT(inputType, outputType, completeness, featureSet) => {
        Success(MapT(inputType, outputType, completeness, featureSet))
      }
      case MapInstance(values) => {
        // TODO - require an explicit conversion here? Maybe this should be left to struct type
        for {
          // TODO - is the convertMapInstanceStructToParams appropriate for cases?
          newParams <- convertMapInstanceStructToParams(values, env)
        } yield {
          val fieldType = {
            Subtype(
              IdentifierT,
              MapInstance(newParams.map(x => IdentifierInstance(x._1) -> Index(1)))
            )
          }
          // PROBLEM!!!!!!! - sometimes it should be case
          StructT(fieldType, objectFound)
        }
      }
      case ParameterObj(name) => {
        //if (env.typeOf(name).isFailure) {
        //  Thread.dumpStack()
        //  env.print()
        //}

        // TODO: put this stuff back in, but we need to make sure the name is in the environment at this point
        /*for {
          typeInfoOfObjectFound <- env.typeOf(name)
          typeOfObjectFound <- typeInfoOfObjectFound match {
            case ExplicitlyTyped(nType) => Success(nType)
            case ImplicitlyTyped(types) => {
              Failure("Param Obj not implemented for ImplicitlyTyped case")
            }
          }

          _ <- Outcome.failWhen(
            !TypeChecker.refersToAType(typeOfObjectFound, env),
            "Could not confirm " + name + " as a type. The elements of type " + typeOfObjectFound.toString + " are not generally types themselves."
          )
        } yield {*/
          Success(SubstitutableT(name))
        //}
      }
      case ApplyFunction(func, input) => {
        for {
          evalInput <- this(NewMapObjectWithType.untyped(input), env)
          evalFunc <- this(NewMapObjectWithType.untyped(func), env)
          functionApplied <- applyFunctionAttempt(evalFunc, evalInput, env)

          result <- functionApplied match {
            case AbleToApplyFunction(nTypeAsObject) => convertObjectToType(nTypeAsObject, env)
            case UnableToApplyDueToUnknownInput => Failure("Unable to Apply Function and get type")
            case NoMatchForInputInFunction => Failure("No Match for function application - shouldn't happen")
          }
        } yield result
      }
      case StructT(fieldType, params) => Success(StructT(fieldType, params))
      case CaseT(params) => Success(CaseT(params))
      case IdentifierInstance(name) => {
        if (name == "T")
          throw new Exception("Identifier " + name + " is not connected to a type.")
        Failure("Identifier " + name + " is not connected to a type.")
      }
      case _ => {
        Failure("Couldn't convert into type: " + objectFound + " -- could be unimplemented")
      }
    }
  }

  def convertMapInstanceStructToParams(
    values: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapType)], String] = {
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