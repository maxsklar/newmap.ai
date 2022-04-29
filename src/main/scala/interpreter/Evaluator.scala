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
          startingType <- CommandMaps.getDefaultValueOfCommandType(evalKeyType, env)
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
      case CountT | TypeT | AnyT | MapT(_, _, _) | StructT(_) | CaseT(_) | OrBooleanT | IdentifierT | ExpandingSubsetT(_, _) | SubtypeT(_) | DataTypeT(_) => Success(UType(nObject))
      case _ => {
        //throw new Exception(nObject.toString)
        Failure(s"Can't yet remove type tag from typed object $nObject (once types are redefined as a case it'll be possible)")
      }
    }
  }

  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)

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

    funcC match {
      case TaggedObject(UMap(values), nType) => {
        for {
          keyMatchResult <- attemptPatternMatchInOrder(values, inputC, env) match {
            case Success(result) => Success(result)
            case Failure(PatternMatchErrorType(message, isEvaluationError)) => {
              if (isEvaluationError) {
                Failure(message)
              } else {
                // Because this is already type checked, we can infer that MapCompleteness == CommandOutput
                // - If it had equaled "MapCompleteness", then we shouldn't be in a situation with no match
                val outputType = RetrieveType.retrieveOutputTypeFromFunctionType(nType, env)
                CommandMaps.getDefaultValueOfCommandType(outputType, env)
              }
            }
          }
        } yield {
          keyMatchResult
        }
      }
      case TaggedObject(IsCommandFunc, _) => {
        val isCommand: Boolean = CommandMaps.getDefaultValueOfCommandType(inputC, env).isSuccess

        Success(Index(if (isCommand) 1 else 0))
      }
      case TaggedObject(IsSimpleFunction, _) => {
        inputC match {
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
      case TaggedObject(IncrementFunc, _) => {
        inputC match {
          case TaggedObject(UIndex(i), nType) => Success(TaggedObject(UIndex(i + 1), nType))
          case _ => Failure(s"Cannot increment $inputC")
        }
      }
      case _ => {
        Failure(s"Not implemented: apply function\nFunction: $func\nInput: $input")
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
            case TaggedObject(UMap(values), DataTypeT(_)) => {
              val uConstructors = values.map(x => x._1 -> ObjectExpression(TaggedObject(UIndex(1), OrBooleanT)))
              val caseConstructorType = TaggedObject(UMap(uConstructors), ExpandingSubsetT(IdentifierT, false))
              val caseMap = TaggedObject(UMap(values), MapT(caseConstructorType, TypeT, MapConfig(RequireCompleteness, BasicMap)))
              Success(caseMap)
            }
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