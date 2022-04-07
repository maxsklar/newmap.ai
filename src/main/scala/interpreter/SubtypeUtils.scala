package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Handles checking if two subtypes are comparable to one another
// and whether an object can be converted to a different type
object SubtypeUtils {
  // For ReqMaps, we need to ensure that all of the values are accounted for.
  // For Maps, we want to know that the default value is never used
  def doPatternsCoverType(
    keys: Vector[NewMapPattern],
    nType: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {

    val objectKeys = keys.flatMap(_ match {
      case ObjectPattern(o) => Some(o)
      case _ => None
    }).toSet

    // This is the generic pattern, which means that everything will match
    // TODO: This is going to get more complicated with more patterns!!
    // - In the future, we want to know if the keys as a group have all the patterns to cover the type
    val genericPatternExists = keys.exists(k => isGenericPattern(k, nType, env))

    if (genericPatternExists) {
      Success(true)
    }
    else {
      val piecemealCompletenessOutcome = nType match {
        case CaseT(cases) => checkCaseComplete(keys, cases, nType, env)
        case StructT(params) => checkStructComplete(keys, params, nType, env)
        case _ => Success(false)
      }

      piecemealCompletenessOutcome match {
        case Success(true) => Success(true)
        case _ => {
          for {
            keysToMatch <- enumerateAllValuesIfPossible(nType, env)
          } yield {
            (keysToMatch -- objectKeys).isEmpty && (objectKeys -- keysToMatch).isEmpty
          }
        }
      }
    }
  }

  def checkCaseComplete(
    keys: Vector[NewMapPattern],
    cases: NewMapObject,
    nType: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    val caseKeys = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(cases), env)

    for {
      // For each case key, we want to make sure that this case key is completely covered
      constructors <- enumerateAllValuesIfPossible(caseKeys, env)
    } yield {
      var returnVal = true

      for {
        constructor <- constructors
      } yield {
        // Look at our keys, and find the ones that are only for this case key, and save those patterns
        val patternsWithThisConstructor = keys.flatMap(key => key match {
          case CasePattern(constructor, pattern) => Some(pattern)
          case ObjectPattern(TaggedObject(UCase(constructor, input), caseT)) => {
            Evaluator.stripVersioning(caseT, env) match {
              case CaseT(cases) => {
                // How do I tag the constructor
                val caseConstructorType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(cases), env)
                val taggedConstructor = TaggedObject(constructor, caseConstructorType)

                // Get the type that the input is supposed to be
                Evaluator.applyFunctionAttempt(caseT, taggedConstructor, env).toOption.map(nType => {
                  ObjectPattern(TaggedObject(input, nType))
                })
              }
              case _ => None
            }
          }
          case _ => None
        })

        // Find the input type for this constructor, and make sure that all of THOSE inputs are accounted for
        Evaluator.applyFunctionAttempt(cases, constructor, env) match {
          case Success(inputType) => {
            doPatternsCoverType(patternsWithThisConstructor, inputType, env) match {
              case Success(false) | Failure(_) => returnVal = false
              case _ => ()
            }
          }
          case _ => {
            returnVal = false
          }
        }
      }

      returnVal
    }
  }

  def checkStructComplete(
    keys: Vector[NewMapPattern],
    cases: NewMapObject,
    nType: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    // TODO: Implement
    Success(false)
  }

  // Returns true if the object is a pattern that will match everything in the type
  def isGenericPattern(pattern: NewMapPattern, nType: NewMapObject, env: Environment): Boolean = {
    pattern match {
      case ObjectPattern(_) => false
      case TypePattern(_, subtype) => isTypeConvertible(nType, subtype, env).toOption.getOrElse(false)
      case StructPattern(patterns)  => {
        nType match {
          // TODO: In the future, maybe we can relax "basicMap" by matching other patterns
          // - That would require isGenericPattern to match an nType that's a NewMapPattern, not just a NewMapObject
          case StructT(TaggedObject(UMap(params), MapT(_, _, _, BasicMap))) if (params.length == patterns.length) => {
            (patterns, params.map(_._2)).zipped.toVector.forall(x => {
              Evaluator(x._2, env).toOption.map(nObject => {
                isGenericPattern(x._1, nObject, env)
              }).getOrElse(false) // We're not really set up for this yet!
            })
          }
          case _ => false 
        }
      }
      case CasePattern(_, _) => false
    }
  }

  def enumerateAllValuesIfPossible(nType: NewMapObject, env: Environment): Outcome[Set[NewMapObject], String] = {
    Evaluator.stripVersioning(nType, env) match {
      // TODO: What if values is too large? Should we make some restrictions here?
      // - Idea: have a value in the environment that gives us a maximum we are allowed to count up to
      case SubtypeT(TaggedObject(UMap(values), _)) => enumerateMapKeys(values.map(_._1))
      case TaggedObject(UIndex(i), _) => {
        Success((0 until i.toInt).map(j => TaggedObject(UIndex(j.toLong), nType)).toSet)
      }
      case _ => Failure(s"Can't enumerate the allowed values of $nType -- could be unimplemented")
    }
  }

  def enumerateMapKeys(values: Vector[NewMapPattern]): Outcome[Set[NewMapObject], String] = {
    values match {
      case value +: additionalValues => {
        value match {
          case ObjectPattern(o) => {
            for {
              addlValues <- enumerateMapKeys(additionalValues)
            } yield addlValues + o
          }
          case _ => Failure(s"Found non-ObjectPattern: $value")
        }
      }
      case _ => Success(Set.empty) 
    }
  }

  // Try to convert an object to having a new type
  // - Fail when the conversion is not possible
  // - If the object already has that type, then just return itself
  def attemptToConvertToType(
    nObject: NewMapExpression,
    requestedType: NewMapObject,
    env: Environment
  ): Outcome[NewMapExpression, String] = {
    val nType = RetrieveType(nObject, env)
    for {
      isConvertible <- isObjectConvertibleToType(nObject, requestedType, env)

      _ <- Outcome.failWhen(
        !isConvertible,
        s"Cannot convert because type of $nObject: $nType doesn't match expected parent type $requestedType."
      )
    } yield nObject
  }

  def doesTypeCoverParentType(nType: NewMapObject, env: Environment): Boolean = nType match {
    case SubtypeT(TaggedObject(UMap(values), MapT(inputType, _, CommandOutput, _))) => {
      // TODO: Extend this to see if pattering matching in basic function covers the full type
      // Check that the function doesn't return the default value for any input
      doPatternsCoverType(values.map(_._1), inputType, env).toOption.getOrElse(false)
    }
    case SubtypeT(IsCommandFunc) => false
    case SubtypeT(IsSimpleFunction) => false
    case SubtypeT(IsVersionedFunc) => false
    case _ => {
      // TODO: Is this appropriate - shouldn't it be false
      true
    }
  }

  // TODO: This can be made to work in much broader circumstances
  // - For example, if the endingType excludes only a finite amount of objects, then we can check
  //    to make sure that all of those are not in startingType
  def isTypeConvertible(
    startingType: NewMapObject,
    endingType: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    val uuidStr = java.util.UUID.randomUUID.toString

    for {
      pureTypeConvertible <- isObjectConvertibleToType(
        ParamId(uuidStr), 
        RetrieveType.getParentType(endingType, env),
        env.newParam(uuidStr, startingType) // TODO - this is an awkward solution! (and it's also wrong.. remove!!)
      )

      _ <- Outcome.failWhen(!pureTypeConvertible, s"Non-convertible pure types from $startingType to $endingType")
    } yield {
      val doesEndtypeCoverParentType = doesTypeCoverParentType(endingType, env)
      val doesStarttypeCoverParentType = doesTypeCoverParentType(startingType, env)

      /// starting typecover parent type

      doesEndtypeCoverParentType || (!doesStarttypeCoverParentType && {
        // End type does not cover parent type
        // So, let's go through all the values of starting type (if we can) and see if we can brute force it
        val allConvertedOutcome = for {
          allValues <- enumerateAllValuesIfPossible(startingType, env)
          doAllConvert <- allMembersOfSubtype(allValues.toVector, endingType, env)
        } yield doAllConvert

        allConvertedOutcome.toOption.getOrElse(false)
      })
    }
  }

  // TODO: ultimately, more potential conversions will be added to the environment, making this function more interesting
  // ALSO: this is for automatic conversion. There should be another conversion, which is a superset of this, which is less automatic
  //  - To be used in cases where you want the programmer to specifically ask for a conversion!
  def isObjectConvertibleToType(
    startingObject: NewMapExpression,
    endingType: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    val startingType = RetrieveType(startingObject, env)

    (startingType, endingType) match {
      case _ if (startingType == endingType) => Success(true)
      case (_, AnyT) => Success(true)
      case (CountT, TypeT) => Success(true)
      case (_, SubtypeT(isMember)) => {
        val subtypeInputType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(isMember), env)

        for {
          // 1: Check if startingObject is convertible to the inputType of isMember
          convertedObject <- attemptToConvertToType(startingObject, subtypeInputType, env)

          // If not evaluated, we can't check for membership
          // TODO: What if this is a complex function?
          // - Solution: only evaluate it if its a simplefunction..
          //   if it's a complex function, leave it alone, and have a good error
          //   message about it saying that we can't check for the subtype because we won't run your
          //   function (which could be some crazy infinite loop or fibonacci explosion crap)
          // Instead.. the function itself should have guarantees
          evaluatedObject <- Evaluator(convertedObject, env)

          // 2: See if it's actually a member of the subtype
          isMemberOfSubtype <- isMemberOfSubtype(evaluatedObject, endingType, env)
          _ <- Outcome.failWhen(!isMemberOfSubtype, s"Object $evaluatedObject not a member of subtype $endingType")
        } yield true
      }
      case (
        MapT(startingInputType, startingOutputType, startingCompleteness, startingFeatureSet),
        MapT(endingInputType, endingOutputType, endingCompleteness, endingFeatureSet)
      ) => {
        // TODO: This is not entirely true
        // I think we can convert these (so long as the feature set is compatible) - but conversion from
        //  CommandOutput might require adding a default pattern
        val isMapCompletenessConvertible = true

        for {
          // Note: the input type is CONTRAvariant, the output type is COvariant, hence the reversal
          // Eventually, we'll have to implement covariance in generic types
          inputTypesConvertible <- isTypeConvertible(endingInputType, startingInputType, env)
          outputTypesConvertible <- isTypeConvertible(startingOutputType, endingOutputType, env)
        } yield {
          inputTypesConvertible &&
          outputTypesConvertible &&
          isFeatureSetConvertible(startingFeatureSet, endingFeatureSet) &&
          isMapCompletenessConvertible
        }
      }
      case(
        StructT(startingParams),
        StructT(endingParams)
      ) => {
        isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(startingParams), env),
          RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(endingParams), env),
          env
        )
        // TODO: The outputs have to agree as well
      }
      case (StructT(mi@TaggedObject(UMap(values), _)), _) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularObj <- Evaluator(singularOutput, env)
          isConvertible <- isTypeConvertible(singularObj, endingType, env)
        } yield isConvertible
      }
      case (_, StructT(mi@TaggedObject(UMap(values), _))) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularObj <- Evaluator(singularOutput, env)
          isConvertible <- isObjectConvertibleToType(startingObject, singularObj, env)
        } yield isConvertible
      }
      case (CaseT(startingCases), CaseT(endingCases)) => {
        // Note the contravariance (ending cases first)
        // This is because a case class with fewer cases can be converted into one with more
        isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(endingCases), env),
          RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(startingCases), env),
          env
        )
        // TODO: The outputs have to agree as well
      }
      case (CaseT(mi@TaggedObject(UMap(values), _)), _) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularObj <- Evaluator(singularOutput, env)
          isConvertible <- isTypeConvertible(singularObj, endingType, env)
        } yield isConvertible
      }
      case (_, CaseT(mi@TaggedObject(UMap(values), _))) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularObj <- Evaluator(singularOutput, env)
          isConvertible <- isObjectConvertibleToType(startingObject, singularObj, env)
        } yield isConvertible
      }
      case (SubtypeT(isMember), _) => {
        val subtypeInputType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(isMember), env)
        
        isTypeConvertible(subtypeInputType, endingType, env)
      }
      case (
        VersionedObjectLink(VersionedObjectKey(versionNumber, uuid), status),
        VersionedObjectLink(VersionedObjectKey(versionNumber2, uuid2), status2)
      ) if (uuid == uuid2) && (status2 == KeepUpToDate) => {
        // Take advantage of the fact that types are backwards compatible
        Success(true)
      }
      case _ => Success(false)
    }
  }

  // I'm pretty sure that this is can be simplified with an ordering!
  def isFeatureSetConvertible(
    startingFeatureSet: MapFeatureSet,
    endingFeatureSet: MapFeatureSet
  ) = startingFeatureSet match {
    case BasicMap => true
    case SimpleFunction => (endingFeatureSet != BasicMap)
    case WellFoundedFunction => (endingFeatureSet != BasicMap && endingFeatureSet != SimpleFunction)
    case FullFunction => (endingFeatureSet == FullFunction)
  }

  // If this function only allows one input, then return the output for that input
  def outputIfFunctionHasSingularInput(nFunction: NewMapObject): Outcome[NewMapExpression, String] = {
    nFunction match {
      case TaggedObject(UMap(values), _) if (values.length == 1) => {
        Success(values.head._2)
      }
      case _ => Failure("Function did not have singular input")
    }
  }

  // Returns true if nObject is a member of nSubtype, assuming that it's already a member of the parent type
  def isMemberOfSubtype(
    nObject: NewMapObject,
    nSubtype: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    // TODO: if nObject is a parameter, we may still be able to confirm that it's in the subtype if the
    // type of it is convertible
    // TODO: We should distinguish between a closed literal - for which we can call quickApplyFunctionAttempt - and non-closed literals

    val apparentTypeOfObject = RetrieveType.fromNewMapObject(nObject, env)

    isTypeConvertible(apparentTypeOfObject, nSubtype, env) match {
      case Success(true) => {
        // Note: This is a shortcut, but not always true
        // - Can we redo isTypeConvertible so it acts purely as a shortcut (maybe a version that doesn't call isPureTypeConvertible)
        Success(true)
      }
      case _ => nSubtype match {
        case SubtypeT(isMember) => {
          for {
            result <- Evaluator.applyFunctionAttempt(isMember, nObject, env)

            // Instead of calling RetrieveType on the result, look at the outputType on nSubtype
            //  and find a default on that!
            defaultValueOrResultType <- Evaluator.getDefaultValueOfCommandType(RetrieveType.fromNewMapObject(result, env), env)
          } yield (result != defaultValueOrResultType)
        }
        case _ => {
          // We already called isTypeConvertible above, so we know this works
          Success(true)
        }
      }
    }
  }

  def allMembersOfSubtype(
    nObjects: Vector[NewMapObject],
    nSubtype: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    nObjects match {
      case nObject +: restOfObjects => {
        for {
          isIt <- isMemberOfSubtype(nObject, nSubtype, env)
          result <- if (isIt) allMembersOfSubtype(restOfObjects, nSubtype, env) else Success(false)
        } yield result
      }
      case _ => Success(true)
    }
  }
}