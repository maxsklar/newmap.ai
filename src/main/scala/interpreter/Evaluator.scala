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
      case Index(_) | CountType | TypeType | IdentifierType | IdentifierInstance(_) | ParameterObj(_) | Increment | AppendToSeq | AppendToMap => {
        Success(nObject)
      }
      case MapType(key, value, default) => {
        for {
          evalKey <- this(NewMapObjectWithType.withTypeE(key, TypeT), env)
          evalValue <- this(NewMapObjectWithType.withTypeE(value, TypeT), env)
          valueType <- convertObjectToType(evalValue, env)
          evalDefault <- this(NewMapObjectWithType.withTypeE(default, valueType), env)
        } yield {
          MapType(evalKey, evalValue, evalDefault)
        }
      }
      case MapInstance(values: Vector[(NewMapObject, NewMapObject)], default) => {
        for {
          evalDefault <- this(NewMapObjectWithType.untyped(default), env)
          evalValues <- evalMapInstanceVals(values, env)
        } yield MapInstance(evalValues, evalDefault)
      }
      case LambdaInstance(lambdaParams, expression) => {
        val newEnv = includeLambdaParams(lambdaParams, env)
        for {
          evalExpression <- this(NewMapObjectWithType.untyped(expression), newEnv)
        } yield {
          LambdaInstance(lambdaParams, evalExpression)
        }
      }
      case LambdaType(inputType, outputType) => {
        for {
          evalInputType <- this(NewMapObjectWithType.untyped(inputType), env)

          // TODO - there are either more cases than this, or this subroutine should be put in its own
          // function
          newEnv = convertObjectToType(evalInputType, env) match {
            case Success(StructT(params)) => env.newParams(params)
            case _ => env
          }

          evalOutputType <- this(NewMapObjectWithType.untyped(outputType), newEnv)
        } yield LambdaType(evalInputType, evalOutputType)
      }
      case ApplyFunction(func, input) => {
        for {
          evalInput <- this(NewMapObjectWithType.untyped(input), env)
          evalFunc <- this(NewMapObjectWithType.untyped(func), env)
          applicationAttempt <- applyFunctionAttempt(evalFunc, evalInput, env)
          result = applicationAttempt match {
            case AbleToApplyFunction(nObject) => nObject
            case UnableToApplyDueToUnknownInput => ApplyFunction(evalFunc, evalInput)
          }
        } yield result
      }
      case StructType(params) => {
        for {
          evalParams <- this(NewMapObjectWithType.untyped(params), env)
        } yield StructType(evalParams)
      }
      case CaseType(params) => {
        for {
          evalParams <- this(NewMapObjectWithType.untyped(params), env)
        } yield CaseType(evalParams)
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
      case SubtypeType(parentType) => {
        for {
          evalParentType <- this(NewMapObjectWithType.withTypeE(parentType, TypeT), env)
        } yield SubtypeType(evalParentType)
      }
      case SubtypeFromMap(map: MapInstance) => {
        for {
          evalMapValues <- evalMapInstanceVals(map.values, env)
          evalMapDefault <- this(NewMapObjectWithType.untyped(map.default), env)
        } yield SubtypeFromMap(MapInstance(evalMapValues, evalMapDefault))
      }
      /*case MutableObject(commands, currentState) => {
        for {
          evalCommands <- evalSequence(commands, env)
          evalCurrentState <- this(NewMapObjectWithType.untyped(currentState), env)
        } yield MutableObject(evalCommands, evalCurrentState)
      }
      case MutableType(staticType, init, commandType, updateFunction) => {
        for {
          evalStaticType <- this(NewMapObjectWithType.withTypeE(staticType, TypeT), env)
          evalInit <- this(NewMapObjectWithType.untyped(init), env)
          evalCommandType <- this(NewMapObjectWithType.withTypeE(commandType, TypeT), env)
          evalUpdateFunction <- this(NewMapObjectWithType.untyped(updateFunction), env)
        } yield MutableType(evalStaticType, evalInit, evalCommandType, evalUpdateFunction)
      }*/
      case IncrementType(baseType) => {
        for {
          evalBaseType <- this(NewMapObjectWithType.withTypeE(baseType, CountT), env)
        } yield IncrementType(evalBaseType)
      }
    }
  }

  def evalSequence(
    values: Vector[NewMapObject],
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = values match {
    case v +: restOfValues => {
      for {
        evalV <- this(NewMapObjectWithType.untyped(v), env)
        evalRest <- evalSequence(restOfValues, env)
      } yield {
        evalV +: evalRest
      }
    }
    case _ => Success(Vector.empty)
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

  def evalParameters(
    params: Vector[(String, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapObject)], String] = params match {
    case (k, v) +: restOfValues => {
      for {
        evalV <- this(NewMapObjectWithType.untyped(v), env)
        evalRest <- evalParameters(restOfValues, env)
      } yield {
        (k -> evalV) +: evalRest
      }
    }
    case _ => Success(Vector.empty)
  }

  sealed abstract class ApplyFunctionAttemptResult
  case class AbleToApplyFunction(nObject: NewMapObject) extends ApplyFunctionAttemptResult
  case object UnableToApplyDueToUnknownInput extends ApplyFunctionAttemptResult

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
          substitutedExpression = makeRelevantSubsitutions(expression, newEnv)
          result <- this(NewMapObjectWithType.untyped(substitutedExpression), env)
        } yield AbleToApplyFunction(result)
      }
      case (LambdaInstance(InputStackParam(typeOfObj), expression), param) => {
        Failure("We don't currently have an input stack: " + expression)
      }
      case (LambdaInstance(StructParams(params), expression), StructInstance(paramValues)) => {
        for {
          newEnv <- updateEnvironmentWithParamValues(params, paramValues, env)
          substitutedExpression = makeRelevantSubsitutions(expression, newEnv)
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
          substitutedExpression = makeRelevantSubsitutions(expression, newEnv)

          paramErasedExpression = if (params.length == 1) {
            substitutedExpression
          } else {
            val newParams = params.drop(1).map(param => {
              param._1 -> makeRelevantSubsitutions(param._2, newEnv)
            })

            LambdaInstance(StructParams(newParams), substitutedExpression)
          }

          result <- this(NewMapObjectWithType.untyped(paramErasedExpression), env)
        } yield AbleToApplyFunction(result)
      }
      case (MapInstance(values, default), key) => {
        for {
          evaluatedKey <- this(NewMapObjectWithType.untyped(key), env)
        } yield {
          evaluatedKey match {
            case ParameterObj(s) => UnableToApplyDueToUnknownInput
            case _ => {
              values.find(_._1 == evaluatedKey).map(_._2) match {
                case Some(result) => AbleToApplyFunction(result)
                case None => AbleToApplyFunction(default)
              }
            }
          }
        }
      }
      case (StructInstance(value: Vector[(String, NewMapObject)]), identifier) => {
        val id = makeRelevantSubsitutions(identifier, env)
        Success(
          AbleToApplyFunction(
            value.find(x => IdentifierInstance(x._1) == id).map(_._2).getOrElse(Index(0))
          )
        )
      }
      case (Increment, Index(i)) => Success(AbleToApplyFunction(Index(i + 1)))
      case (Increment, _) => Success(UnableToApplyDueToUnknownInput)
      case (AppendToSeq, StructInstance(paramValues)) => {
        /**
        params = Vector(
          "currentSize" -> CountT,
          "valueType" -> TypeT,
          "defaultValue" -> SubstitutableT("valueType"),
          "currentSeq" -> MapT(SubstitutableT("currentSize"), SubstitutableT("valueType"), ParameterObj("defaultValue")),
          "nextValue" -> SubstitutableT("valueType")
        ),
        */
        Failure("Not implemented: Apply function AppendToSeq")
      }
      case (AppendToMap, StructInstance(paramsValues)) => {
        /**
          params = Vector(
            "keyType" -> TypeT,
            "valueType" -> TypeT,
            "default" -> SubstitutableT("valueType"),
            "currentMap" -> MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), ParameterObj("default")),
            "appendedMap" -> MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), ParameterObj("default"))
          ),
        */
        Failure("Not implemented: Apply function AppendToMap")
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

  def makeRelevantSubsitutions(
    expression: NewMapObject,
    env: Environment
  ): NewMapObject = {
    expression match {
      case Index(_) | CountType | TypeType | IdentifierType | IdentifierInstance(_) | Increment | AppendToSeq | AppendToMap=> expression
      case MapType(key, value, default) => {
        MapType(makeRelevantSubsitutions(key, env), makeRelevantSubsitutions(value, env), makeRelevantSubsitutions(default, env))
      }
      case MapInstance(values, default) => {
        val newValues = for {
          (k, v) <- values
        } yield (makeRelevantSubsitutions(k, env) -> makeRelevantSubsitutions(v, env))

        MapInstance(newValues, default)
      }
      case LambdaType(inputType, outputType) => {
        LambdaType(
          makeRelevantSubsitutions(inputType, env),
          makeRelevantSubsitutions(outputType, env)
        )
      }
      case LambdaInstance(params, expression) => {
        val newEnv = includeLambdaParams(params, env)
        val newExpression = makeRelevantSubsitutions(expression, newEnv)
        LambdaInstance(params, newExpression)
      }
      case ApplyFunction(func, input) => {
        ApplyFunction(
          makeRelevantSubsitutions(func, env),
          makeRelevantSubsitutions(input, env)
        )
      }
      case ParameterObj(name) => {
        env.objectOf(name) match {
          case Some(obj) => obj
          case None => expression
        }
      }
      case StructType(values) => {
        StructType(makeRelevantSubsitutions(values, env))
      }
      case CaseType(values) => {
        CaseType(makeRelevantSubsitutions(values, env))
      }
      case StructInstance(value) => {
        StructInstance(value.map(x => (x._1 -> makeRelevantSubsitutions(x._2, env))))
      }
      case CaseInstance(constructor, value) => {
        CaseInstance(constructor, makeRelevantSubsitutions(value, env))      
      }
      case SubtypeType(parentType) => {
        SubtypeType(makeRelevantSubsitutions(parentType, env))
      }
      case SubtypeFromMap(MapInstance(values, default)) => {
        val newValues = for {
          (k, v) <- values
        } yield (makeRelevantSubsitutions(k, env) -> makeRelevantSubsitutions(v, env))

        val newMapInstance = MapInstance(newValues, default)

        SubtypeFromMap(newMapInstance)
      }
      /*case MutableObject(commands, currentState) => {
        MutableObject(
          commands.map(command => makeRelevantSubsitutions(command, env)),
          makeRelevantSubsitutions(currentState, env)
        )
      }
      case MutableType(staticType, init, commandType, updateFunction) => {
        MutableType(
          makeRelevantSubsitutions(staticType, env),
          makeRelevantSubsitutions(init, env),
          makeRelevantSubsitutions(commandType, env),
          makeRelevantSubsitutions(updateFunction, env)
        )
      }*/
      case IncrementType(baseType) => IncrementType(makeRelevantSubsitutions(baseType, env))
    }
  }

  def includeLambdaParams(
    lambdaParams: LambdaParamStrategy,
    env: Environment
  ): Environment = {
    lambdaParams match {
      case StructParams(params) => includeParams(params, env)
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
  def convertObjectToType(
    objectFound: NewMapObject,
    env: Environment
  ): Outcome[NewMapType, String] = {
    objectFound match {
      case Index(i) => Success(IndexT(i))
      case TypeType => Success(TypeT)
      case CountType => Success(CountT)
      case IdentifierType => Success(IdentifierT)
      case MapType(key, value, default) => {
        for {
          keyType <- convertObjectToType(key, env) 
          valueType <- convertObjectToType(value, env)
        } yield {
          MapT(keyType, valueType, default)
        }
      }
      case LambdaType(inputType, outputType) => {
        for {
          evalInputType <- convertObjectToType(inputType, env)

          newEnv = evalInputType match {
            case StructT(params) => env.newParams(params)

            // TODO: there might be other options here
            case _ => env
          }

          // TODO - the end in the output type is affected by the input type
          evalOutputType <- convertObjectToType(outputType, newEnv)
        } yield {
          LambdaT(evalInputType, evalOutputType)
        }
      }
      case MapInstance(values, Index(1)) => {
        // TODO - require an explicit conversion here? Maybe this should be left to struct type
        for {
          // This must be a identifier -> type map
          newParams <- convertMapInstanceStructToParams(values, env)
        } yield {
          StructT(newParams)
        }
      }
      case MapInstance(values, Index(0)) => {
        // TODO - require an explicit conversion here? Maybe this should be left to struct type
        for {
          // TODO - is the convertMapInstanceStructToParams appropriate for cases?
          newParams <- convertMapInstanceStructToParams(values, env)
        } yield {
          CaseT(newParams)
        }
      }
      case ParameterObj(name) => {
        for {
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
        } yield {
          SubstitutableT(name)
        }
      }
      case ApplyFunction(Increment, input) => {
        for {
          evalInput <- this(NewMapObjectWithType.untyped(input), env)
          inputType <- convertObjectToType(evalInput, env)
        } yield IncrementT(inputType)
      }
      case ApplyFunction(func, input) => {
        for {
          evalInput <- this(NewMapObjectWithType.untyped(input), env)
          evalFunc <- this(NewMapObjectWithType.untyped(func), env)
          functionApplied <- applyFunctionAttempt(evalFunc, evalInput, env)

          result <- functionApplied match {
            case AbleToApplyFunction(nTypeAsObject) => convertObjectToType(nTypeAsObject, env)
            case UnableToApplyDueToUnknownInput => Failure("Cannot convert applied function to type " + objectFound)
          }
        } yield result
      }
      case StructType(params) => {
        for {
          mapInstance <- this(NewMapObjectWithType.untyped(params), env)

          values <- mapInstance match {
            case MapInstance(v, Index(1)) => Success(v)
            case _ => Failure("Map Instance could not be resolved")
          }

          newParams <- convertMapInstanceStructToParams(values, env)
        } yield {
          StructT(newParams)
        }
      }
      case CaseType(params) => {
        // TODO - this is repeated code from StructType
        for {
          mapInstance <- this(NewMapObjectWithType.untyped(params), env)

          values <- mapInstance match {
            case MapInstance(v, Index(0)) => Success(v)
            case MapInstance(v, default) => Failure("Map Instance " + mapInstance + " has the wrong default: " + default)
            case _ => Failure("Map Instance could not be resolved for params " + params + "\nInstead recieved: " + mapInstance)
          }

          newParams <- convertMapInstanceStructToParams(values, env)
        } yield {
          CaseT(newParams)
        }
      }
      /*case MutableType(staticType, init, commandType, updateFunction) => {
        for {
          staticT <- convertObjectToType(staticType, env)
          commandT <- convertObjectToType(commandType, env)
        } yield {
          MutableT(staticT, init, commandT, updateFunction)
        }
      }*/
      case IdentifierInstance(name) => {
        Failure("Identifier " + name + " is not connected to a type.")
      }
      case _ => {
        // TODO: Need to explicitly handle every case
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