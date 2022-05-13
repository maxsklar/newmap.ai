package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Evaluates an expression that's already been type checked
object Evaluator {
  def apply(
    nExpression: NewMapExpression,
    env: Environment
  ): Outcome[UntaggedObject, String] = {
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
          case Some(EnvironmentBinding(nObject)) => removeTypeTag(nObject)
          case Some(EnvironmentParameter(nObject)) => {
            //throw new Exception(s"Cannot evaluate identifier $s, since it is an unbound parameter of type $nObject")
            Failure(s"Cannot evaluate identifier $s, since it is an unbound parameter of type $nObject")
          }
        }
      }
      case BuildCase(constructor, input) => {
        for {
          evalInput <- this(input, env)
        } yield {
          UCase(constructor, evalInput)
        }
      }
      case BuildMapT(inputType, outputType, config) => {
        for {
          evalInputType <- this(inputType, env)
          inputT <- asType(evalInputType, env)

          evalOutputType <- this(outputType, env)
          outputT <- asType(evalOutputType, env)
        } yield {
          UType(MapT(inputT, outputT, config))
        }
      }
      case BuildGenericMapT(typeTransform, config) => {
        for {
          evalTypeTransform <- this(typeTransform, env)

          evalTypeTransformM <- evalTypeTransform match {
            case UMap(m) => Success(m)
            case _ => Failure(s"Unexpected type transform: $evalTypeTransform")
          }
        } yield {
          UType(GenericMapT(evalTypeTransformM, config))
        }
      }
      case BuildTableT(keyType, requiredValues) => {
        for {
          evalKeyType <- this(keyType, env)
          startingT <- asType(evalKeyType, env)

          evalRequiredValues <- this(requiredValues, env)
          valueT <- asType(evalRequiredValues, env)
        } yield {
          // TODO - are we going to know that this is an expandable type?
          // - I think so because startingType is tagged!!
          UType(MapT(
            startingT,
            valueT,
            MapConfig(RequireCompleteness, SimpleFunction)
          ))
        }
      }
      case BuildSubtypeT(isMember, parentType, featureSet) => {
        for {
          evalIsMember <- this(isMember, env)
          evalParentType <- this(parentType, env)
          evalParentT <- asType(evalParentType, env)
        } yield UType(SubtypeT(evalIsMember, evalParentT, featureSet))
      }
      case BuildCaseT(cases, parentFieldType, featureSet) => {
        for {
          evalCases <- this(cases, env)
          evalCasesM <- evalCases match {
            case UMap(m) => Success(m)
            case _ => Failure(s"Unexpected case map: $evalCases")
          }
        } yield UType(CaseT(evalCasesM, parentFieldType, featureSet))
      }
      case BuildStructT(params, parentFieldType, completeness, featureSet) => {
        for {
          evalParams <- this(params, env)

          evalStructM <- evalParams match {
            case UMap(m) => Success(m)
            case _ => Failure(s"Unexpected struct map: $evalParams")
          }
        } yield {
          UType(StructT(evalStructM, parentFieldType, completeness, featureSet))
        }
      }
      case BuildNewTypeClassT(typeTransform) => {
        for {
          evalTypeTransform <- this(typeTransform, env)

          evalTypeTransformM <- evalTypeTransform match {
            case UMap(m) => Success(m)
            case _ => Failure(s"Unexpected type transform: $evalTypeTransform")
          }
        } yield {
          UType(TypeClassT(evalTypeTransformM, Vector.empty))
        }
      }
      case BuildMapInstance(values) => {
        for {
          evalValues <- evalMapInstanceVals(values, env)
        } yield UMap(values)
      }
    }
  }

  // TODO - try to remove the usage of this as much as possible
  // (In other words, look for ways to rely on the type less)
  def removeTypeTag(nObject: NewMapObject): Outcome[UntaggedObject, String] = {
    nObject match {
      case TaggedObject(uObject, _) => Success(uObject)
      case VersionedObjectLink(key, _) => Success(ULink(key))
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

      versionedO <- versionedObject match {
        case EnvironmentBinding(vo@VersionedObjectLink(_, _)) => Success(vo)
        case EnvironmentBinding(nObject) => Failure(s"Identifier $id does not point to a versioned object. It is actually ${nObject}.")
        case EnvironmentParameter(_) => Failure(s"Identifier $id is a parameter, should be an object")
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
  def applyFunctionAttempt(
    func: UntaggedObject,
    input: UntaggedObject,
    env: Environment
  ): Outcome[UntaggedObject, String] = {
    stripVersioningU(func, env) match {
      case UMap(values) => {
        for {
          keyMatchResult <- attemptPatternMatchInOrder(values, input, env) match {
            case Success(result) => Success(result)
            case Failure(PatternMatchErrorType(message, isEvaluationError)) => {
              if (isEvaluationError) {
                Failure(message)
              } else {
                // Because this is already type checked, we can infer that MapCompleteness == CommandOutput
                // - If it had equaled "MapCompleteness", then we shouldn't be in a situation with no match
                Success(UInit)
              }
            }
          }
        } yield {
          keyMatchResult
        }
      }
      case IsCommandFunc => {
        val defaultValueOutcome = for {
          defaultValue <- CommandMaps.getDefaultValueOfCommandType(input, env)
        } yield defaultValue

        val isCommand: Boolean = defaultValueOutcome.isSuccess

        Success(if (isCommand) UIndex(1) else UInit)
      }
      case IncrementFunc => {
        input match {
          case UIndex(i) => Success(UIndex(i + 1))
          case _ => Failure(s"Cannot increment $input")
        }
      }
      case _ => {
        //throw new Exception(s"Not implemented: apply function\nFunction: $func\nInput: $input")
        Failure(s"Not implemented: apply function\nFunction: $func\nInput: $input")
      }
    }
  }

  // TODO - there should be no evaluation errors
  // This is only to catch non-constant values, which cannot be checked by the type checker yet
  case class PatternMatchErrorType(message: String, isEvaluationError: Boolean)

  def attemptPatternMatchInOrder(
    remainingPatterns: Vector[(NewMapPattern, NewMapExpression)],
    input: UntaggedObject,
    env: Environment
  ): Outcome[UntaggedObject, PatternMatchErrorType] = {
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

  def attemptPatternMatch(
    pattern: NewMapPattern,
    input: UntaggedObject,
    env: Environment
  ): Outcome[Map[String, UntaggedObject], String] = {
    // TODO: IMPORTANT
    // We must be able to deal with using the same variable in a pattern, like StructPattern(x, x) to
    //  denote that these are the same
    (pattern, stripVersioningU(input, env)) match {
      case (StructPattern(params), UMap(paramValues)) => {
        for {
          inputs <- expressionListToObjects(paramValues.map(_._2), env)
          result <- patternMatchOnStruct(params, inputs, env)
        } yield result 
      }
      case (WildcardPattern(name), _) => {
        Success(Map(name -> input))
      }
      // TODO - eventually instead of checking equality, we'll check for "convertability"
      //  For example between different type versions
      case (ObjectPattern(oPattern), _) => {
        if (SubtypeUtils.checkEqual(oPattern, input)) {
          Success(Map.empty)
        } else Failure("ObjectPattern didn't match")
      }
      case (CasePattern(constructorP, inputP), UCase(constructor, cInput)) => {
        for {
          _ <- Outcome.failWhen(!SubtypeUtils.checkEqual(constructorP, constructor), "Constructors didn't match")
          result <- attemptPatternMatch(inputP, cInput, env)
        } yield result
      }
      case _ => {
        Failure("Failed Pattern Match")
      }
    }
  }

  def patternMatchOnStruct(
    structPattern: Vector[NewMapPattern],
    inputs: Vector[UntaggedObject],
    env: Environment
  ): Outcome[Map[String, UntaggedObject], String] = {
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

  def stripVersioningU(uObject: UntaggedObject, env: Environment): UntaggedObject = {
    uObject match {
      case ULink(key) => removeTypeTag(currentState(key.uuid, env).toOption.get).toOption.get
      case _ => uObject
    }
  }

  def asType(uObject: UntaggedObject, env: Environment): Outcome[NewMapType, String] = {
    stripVersioningU(uObject, env) match {
      case UType(t) => Success(t)
      case UIndex(j) => Success(IndexT(j))
      case other => {
        //throw new Exception(s"Not a type: $uObject")
        Failure(s"Not a type: $other")
      }
    }
  }
}