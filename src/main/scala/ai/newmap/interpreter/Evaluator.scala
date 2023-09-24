package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Evaluates an expression that's already been type checked
object Evaluator {
  def apply(
    nExpression: UntaggedObject,
    env: Environment,

    // TODO: any way we can get rid of this?
    paramsToAllow: Vector[String] = Vector.empty
  ): Outcome[UntaggedObject, String] = {
    nExpression match {
      case ApplyFunction(func, input, matchingRules) => {
        for {
          evalFunc <- this(func, env, paramsToAllow)

          evalInput <- this(input, env, paramsToAllow)

          result <- applyFunction(evalFunc, evalInput, env, matchingRules)
        } yield result
      }
      case AccessField(value, uTypeClass, field) => {
        for {
          evalValue <- this(value, env, paramsToAllow)

          // The field and type class should already be evaluated, so no need to re-evaluate it here
          fieldsToMap <- applyFunction(env.typeToFieldMapping, uTypeClass, env, TypeMatcher)

          result <- applyFunction(fieldsToMap, field, env)

          resultValue <- result match {
            case UCase(_, v) => Success(v)
            case _ => Failure("Can't access value: " + result)
          }
          answer <- applyFunction(resultValue, evalValue, env)
        } yield {
          answer
        }
      }
      case ParamId(s) => {
        env.lookupValue(s) match {
          case Some(nObject) => Success(nObject.uObject)
          case None => if (paramsToAllow.contains(s)) Success(ParamId(s)) else {
            Failure(s"Unbound identifier: $s")
          }
        }
      }
      case UCase(constructor, input) => {
        for {
          evalInput <- this(input, env, paramsToAllow)
        } yield {
          UCase(constructor, evalInput)
        }
      }
      case UMap(values) => {
        for {
          evalValues <- evalMapInstanceVals(values, env, paramsToAllow)
        } yield UMap(evalValues)
      }
      case UMapPattern(key, value) => {
        for {
          evalKey <- this(key, env, paramsToAllow)

          newParams = newParametersFromPattern(evalKey)

          // TODO - use "params to allow" in this
          evalValue = this(value, env, paramsToAllow ++ newParams) match {
            case Success(vObj) => vObj
            case Failure(f) => value
          }
        } yield UMapPattern(evalKey, evalValue)
      }
      case UStruct(values) => {
        for {
          evalValues <- evalStructVals(values, env, paramsToAllow)
        } yield UStruct(evalValues)
      }
      case ULet(statements, expression) => {
        evalLet(statements, expression, env, paramsToAllow)
      }
      case constant => Success(constant)
    }
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

  def evalMapInstanceVals(
    values: Vector[(UntaggedObject, UntaggedObject)],
    env: Environment,
    paramsToAllow: Vector[String]
  ): Outcome[Vector[(UntaggedObject, UntaggedObject)], String] = {
    values match {
      case (k, v) +: restOfValues => {
        //val newParams = newParametersFromPattern(k)
        val newParams = Vector.empty

        // TODO - rethink about "when to evaluate" what's inside a map
        val newV = this(v, env, paramsToAllow ++ newParams).toOption.getOrElse(v)

        for {
          evalRest <- evalMapInstanceVals(restOfValues, env, paramsToAllow)
        } yield (k -> newV) +: evalRest
      }
      case _ => Success(Vector.empty)
    }
  }

  def evalStructVals(
    values: Vector[UntaggedObject],
    env: Environment,
    paramsToAllow: Vector[String]
  ): Outcome[Vector[UntaggedObject], String] = {
    values match {
      case v +: restOfValues => {
        for {
          evalRest <- evalStructVals(restOfValues, env, paramsToAllow)
          evalV <- this(v, env, paramsToAllow)
        } yield {
          evalV +: evalRest
        }
      }
      case _ => Success(Vector.empty)
    }
  }

  def evalLet(
    commands: Vector[EnvironmentCommand],
    expression: UntaggedObject,
    env: Environment,
    paramsToAllow: Vector[String]
  ): Outcome[UntaggedObject, String] = commands match {
    case command +: otherCommands => {
      // If there are still parameters in the command, it cannot be evaluated
      val newEnv = env.newCommand(command)
      evalLet(otherCommands, expression, newEnv, paramsToAllow)
    }
    case _ => {
      for {
        result <- this(expression, env, paramsToAllow)
      } yield {
        stripVersioningU(result, env)
      }
    }
  }

  def expressionListToObjects(
    nExpressions: Vector[UntaggedObject],
    env: Environment
  ): Outcome[Vector[UntaggedObject], String] = {
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
  def applyFunction(
    func: UntaggedObject,
    input: UntaggedObject,
    env: Environment,
    matchingRules: MatchingRules = StandardMatcher
  ): Outcome[UntaggedObject, String] = {
    stripVersioningU(func, env) match {
      case UMap(values) => {
        val keyMatchResult = patternMatchInOrder(values, input, env, matchingRules) match {
          case Success(result) => result
          case Failure(f) => {
            // Because this is already type checked, we can infer that MapCompleteness == CommandOutput
            // - If it had equaled "MapCompleteness", then we shouldn't be in a situation with no match
            UInit
          }
        }

        this(keyMatchResult, env)
      }
      case UMapPattern(key, value) => {
        // Treat it like a small map
        applyFunction(UMap(Vector(key -> value)), input, env, matchingRules)
      }
      case IsCommandFunc => {
        val defaultValueOutcome = for {
          inputT <- input.asType
          defaultValue <- CommandMaps.getDefaultValueOfCommandType(inputT, env)
        } yield defaultValue

        val isCommand: Boolean = defaultValueOutcome.isSuccess

        Success(if (isCommand) UIndex(1) else UInit)
      }
      case UFunctionLink(name, _) => {
        for {
          // TODO - don't ignore the uuid if we're using an old functional system
          fSystemV <- env.lookupVersionedObject("__FunctionSystem")
          uFunction <- applyFunction(ULink(fSystemV.key), name, env)
          result <- applyFunction(uFunction, input, env)
        } yield result
      }
      case UPlus => {
        for {
          first <- applyFunction(input, UIndex(0), env)
          second <- applyFunction(input, UIndex(1), env)

          result <- (first, second) match {
            case (UIndex(n1), UIndex(n2)) => Success(UIndex(n1 + n2))
            case (UDouble(d1), UDouble(d2)) => Success(UDouble(d1 + d2))
            case _ => Failure("Can't add: " + first + " -- " + second)
          }
        } yield result
      }
      case UTimes => {
        for {
          first <- applyFunction(input, UIndex(0), env)
          second <- applyFunction(input, UIndex(1), env)

          result <- (first, second) match {
            case (UIndex(n1), UIndex(n2)) => Success(UIndex(n1 * n2))
            case (UDouble(d1), UDouble(d2)) => Success(UDouble(d1 * d2))
            case _ => Failure("Can't add: " + first + " -- " + second)
          }
        } yield result
      }
      case UDivide => {
        for {
          first <- applyFunction(input, UIndex(0), env)
          second <- applyFunction(input, UIndex(1), env)

          result <- (first, second) match {
            case (UDouble(d1), UDouble(d2)) => Success(UDouble(d1 / d2))
            case _ => Failure("Can't add: " + first + " -- " + second)
          }
        } yield result
      }
      case UCountToDecimal => input match {
        case UIndex(i) => Success(UDouble(i.toDouble))
        case _ => Failure("Can't convert to decimal: " + input)
      }
      case _ => {
        Failure(s"Not implemented: apply function\nFunction: $func\nInput: $input")
      }
    }
  }

  def patternMatchInOrder(
    remainingPatterns: Vector[(UntaggedObject, UntaggedObject)],
    input: UntaggedObject,
    env: Environment,
    matchingRules: MatchingRules = StandardMatcher
  ): Outcome[UntaggedObject, String] = {
    remainingPatterns match {
      case (pattern, answer) +: addlPatterns => {
        patternMatch(pattern, input, matchingRules, env) match {
          case Success(paramsToSubsitute) => {
            Success(MakeSubstitution(answer, paramsToSubsitute))
          }
          case Failure(_) => patternMatchInOrder(addlPatterns, input, env, matchingRules)
        }
      }
      case _ => Failure(
        s"Unable to pattern match $input, The type checker should have caught this so there may be an error in there",
      )
    }
  }


  def patternMatch(
    pattern: UntaggedObject,
    input: UntaggedObject,
    matchingRules: MatchingRules, // See comment in type definition
    env: Environment
  ): Outcome[Map[String, UntaggedObject], String] = {
    pattern match {
      // Always look for wildcard first, no matter what the matching rules are!
      case UWildcardPattern(name) => Success(Map(name -> input))
      case UMapPattern(key, value) => {
        for {
          valueForKey <- applyFunction(input, key, env, matchingRules)
          result <-  patternMatch(value, valueForKey, matchingRules, env)
        } yield result
      }
      case _ => matchingRules match {
        case StandardMatcher => standardPatternMatch(pattern, input, env)
        case TypeMatcher => {
          for {
            patternT <- pattern.asType
            inputT <- input.asType
            response <- TypeConversionCalculator.isTypeConvertible(inputT, patternT, env)
          } yield {
            // Do we make use of the conversion rules at all?
            response.newParameters
          }
        }
      }
    }
  }

  // TODO: IMPORTANT
  // We must be able to deal with using the same variable in a pattern, like StructPattern(x, x) to
  //  denote that these are the same... Should only be available in struct and map patterns
  def standardPatternMatch(
    pattern: UntaggedObject,
    input: UntaggedObject,
    env: Environment
  ): Outcome[Map[String, UntaggedObject], String] = {
    (pattern, stripVersioningU(input, env)) match {
      case (UStruct(params), UMap(paramValues)) => {
        for {
          inputs <- expressionListToObjects(paramValues.map(_._2), env)
          result <- patternMatchOnStruct(params, inputs, env)
        } yield result 
      }
      case (UMap(paramValues), UStruct(params)) => {
        for {
          inputs <- expressionListToObjects(paramValues.map(_._2), env)
          result <- patternMatchOnStruct(inputs, params, env)
        } yield result 
      }
      case (UStruct(params), UStruct(inputParams)) => {
        patternMatchOnStruct(params, inputParams, env)
      }
      case (UStruct(params), singleValue) if (params.length == 1) => {
        patternMatch(params.head, singleValue, StandardMatcher, env)
      }
      case (UCase(constructorP, inputP), UCase(constructor, cInput)) => {
        for {
          resultConstructors <- patternMatch(constructorP, constructor, StandardMatcher, env)
          result <- patternMatch(inputP, cInput, StandardMatcher, env)
        } yield (resultConstructors ++ result)
      }
      case (UCase(UIdentifier("Inc"), i), UIndex(j)) if j > 0 => {
        // This is the kind of thing that needs to be in a custom matcher for counts!!
        patternMatch(i, UIndex(j - 1), StandardMatcher, env)
      }
      case (_, UWildcardPattern(wildcard)) => {
        //throw new Exception(s"Failed Pattern Match: Split wildcard $wildcard on $pattern")
        Failure(s"Failed Pattern Match: Split wildcard $wildcard on $pattern")
      }
      case (oPattern, strippedInput) => {
        // TODO - instead of checking for equality here - go through each untagged object configuration
        if (oPattern == strippedInput) {
          Success(Map.empty)
        } else {
          Failure(s"Failed Pattern Match: $oPattern -- $strippedInput")
        }
      }
    }
  }

  def patternMatchOnStruct(
    structPattern: Vector[UntaggedObject],
    inputs: Vector[UntaggedObject],
    env: Environment
  ): Outcome[Map[String, UntaggedObject], String] = {
    (structPattern, inputs) match {
      case (firstPattern +: restOfPatterns, firstInput +: restOfInputs) => {
        for {
          newParameters <- patternMatch(firstPattern, firstInput, StandardMatcher, env)
          otherParameters <- patternMatchOnStruct(restOfPatterns, restOfInputs, env)
        } yield {
          newParameters ++ otherParameters
        }
      }
      case _ if (structPattern.isEmpty && inputs.isEmpty) => Success(Map.empty) // No patterns to match
      case _ => {
        Failure(s"patternMatchOnStruct: structPattern and inputPattern are of different length. Leftovers: $structPattern -- $inputs")
      }  
    }
  }

  // TODO - move this elsewhere, maybe to environment!
  def newParametersFromPattern(pattern: UntaggedObject): Vector[String] = pattern match {
    case UWildcardPattern(name) => Vector(name)
    case UStruct(patterns) => patterns match {
      case firstPattern +: otherPatterns => {
        newParametersFromPattern(firstPattern) ++ newParametersFromPattern(UStruct(otherPatterns))
      }
      case _ => Vector.empty
    }
    case UCase(_, input) => {
      newParametersFromPattern(input)
    }
    case UMap(values) => newParametersFromPattern(UStruct(values.map(_._2)))
    case _ => Vector.empty
  }

  def applyListOfFunctions(
    original: UntaggedObject,
    listOfFunctions: Vector[FunctionWithMatchingRules],
    env: Environment
  ): Outcome[UntaggedObject, String] = {
    listOfFunctions match {
      case instruction +: followingInstructions => {
        for {
          newObject <- applyFunction(instruction.func, original, env, instruction.matchingRules)
          result <- applyListOfFunctions(newObject, followingInstructions, env)
        } yield result
      }
      case _ => Success(original)
    }
  }

  def stripVersioningU(uObject: UntaggedObject, env: Environment): UntaggedObject = {
    uObject match {
      case ULink(key) => currentState(key.uuid, env).toOption.get.uObject
      case _ => uObject
    }
  }
}