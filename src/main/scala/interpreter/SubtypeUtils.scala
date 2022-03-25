package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Handles checking if two subtypes are comparable to one another
// and whether an object can be converted to a different type
object SubtypeUtils {
  // For ReqMaps, we need to ensure that all of the values are accounted for.
  // For Maps, we want to know that the default value is never used
  def doMapValuesCoverType(
    values: Vector[(NewMapPattern, NewMapObject)],
    nType: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    val keys = values.map(_._1).toSet

    val objectKeys = values.flatMap(v => v._1 match {
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
      for {
        keysToMatch <- enumerateAllValuesIfPossible(nType)
      } yield {
        (keysToMatch -- objectKeys).isEmpty && (objectKeys -- keysToMatch).isEmpty
      }
    }
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
          case StructT(MapInstance(params, MapT(_, _, _, BasicMap))) if (params.length == patterns.length) => {
            (patterns, params.map(_._2)).zipped.toVector.forall(x => {
              isGenericPattern(x._1, x._2, env)
            })
          }
          case _ => false 
        }
      }
    }
  }

  def enumerateAllValuesIfPossible(nType: NewMapObject): Outcome[Set[NewMapObject], String] = {
    nType match {
      // TODO: What if values is too large? Should we make some restrictions here?
      // - Idea: have a value in the environment that gives us a maximum we are allowed to count up to
      case SubtypeT(MapInstance(values, _)) => enumerateMapKeys(values.map(_._1))
      // TODO(2022): if simpleFunction is boolean function on a small finite parentType, we should be able to enumerate those
      // TODO(2022): this is also one of those advanced cases where we want to know if one set is a subset of another through some advanced argument (like monotonicity)
      // TODO - structs and cases
      case StructT(mi@MapInstance(values, _)) if (values.length == 1) => {
        // TODO: we may also be able to enumerate all values if the length > 1 !
        // ALSO - we should have a better way to deal with all these singular value checks
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          result <- enumerateAllValuesIfPossible(singularOutput)
        } yield result
      }
      case Index(i) => {
        Success((0 until i.toInt).map(j => IndexValue(j.toLong, Index(i))).toSet)
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
    nObject: NewMapObject,
    requestedType: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    val nType = RetrieveType(nObject, env)

    if (nType != requestedType) {
      for {
        isConvertible <- isObjectConvertibleToType(nObject, requestedType, env)
        _ <- Outcome.failWhen(
          !isConvertible,
          s"Cannot convert because type of $nObject: $nType doesn't match expected parent type $requestedType."
        )
      } yield nObject
    } else {
      Success(nObject)
    }
  }

  //Case (Cons~Id: Struct (head~Id: Count, tail~Id: VER[e61ed3eb-9131-46e8-8d27-fdabade5091a]), Nil~Id: Struct ())

  def doesTypeCoverParentType(nType: NewMapObject, env: Environment): Boolean = nType match {
    case SubtypeT(MapInstance(values, MapT(inputType, _, CommandOutput, _))) => {
      // TODO: Extend this to see if pattering matching in basic function covers the full type
      // Check that the function doesn't return the default value for any input
      doMapValuesCoverType(values, inputType, env).toOption.getOrElse(false)
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
    for {
      pureTypeConvertible <- isObjectConvertibleToType(
        ParameterObj(java.util.UUID.randomUUID, startingType), // TODO - this is an awkward solution! (and it's also wrong.. remove!!)
        RetrieveType.getParentType(endingType, env),
        env
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
          allValues <- enumerateAllValuesIfPossible(startingType)
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
    startingObject: NewMapObject,
    endingType: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    val startingType = RetrieveType(startingObject, env)

    (startingType, endingType) match {
      case _ if (startingType == endingType) => Success(true)
      case (_, AnyT) => Success(true)
      case (CountT, TypeT) => Success(true)
      case (_, SubtypeT(isMember)) => {
        val subtypeInputType = RetrieveType.retrieveInputTypeFromFunction(isMember, env)

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

        val inputTypesConvertible = {
          //isTypeConvertible(endingInputType, startingInputType, env)
          // TODO - we're going to have some trouble with this in type land until generics come around
          true
        }
        val outputTypesConvertible = {
          // Same  
          // isTypeConvertible(startingOutputType, endingOutputType, env)
          true
        }

        // Note: the input type is CONTRAvariant, the output type is COvariant, hence the reversal
        // Eventually, we'll have to implement covariance in generic types
        Success(
          inputTypesConvertible &&
          outputTypesConvertible &&
          isFeatureSetConvertible(startingFeatureSet, endingFeatureSet) &&
          isMapCompletenessConvertible
        )
      }
      case(
        StructT(startingParams),
        StructT(endingParams)
      ) => {
        isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunction(startingParams, env),
          RetrieveType.retrieveInputTypeFromFunction(endingParams, env),
          env
        )
        // TODO: The outputs have to agree as well
      }
      case (StructT(mi@MapInstance(values, _)), _) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          isConvertible <- isTypeConvertible(singularOutput, endingType, env)
        } yield isConvertible
      }
      case (_, StructT(mi@MapInstance(values, _))) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          isConvertible <- isObjectConvertibleToType(startingObject, singularOutput, env)
        } yield isConvertible
      }
      case (CaseT(startingCases), CaseT(endingCases)) => {
        // Note the contravariance (ending cases first)
        // This is because a case class with fewer cases can be converted into one with more
        isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunction(endingCases, env),
          RetrieveType.retrieveInputTypeFromFunction(startingCases, env),
          env
        )
        // TODO: The outputs have to agree as well
      }
      case (CaseT(mi@MapInstance(values, _)), _) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          isConvertible <- isTypeConvertible(singularOutput, endingType, env)
        } yield isConvertible
      }
      case (_, CaseT(mi@MapInstance(values, _))) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          isConvertible <- isObjectConvertibleToType(startingObject, singularOutput, env)
        } yield isConvertible
      }
      case (SubtypeT(isMember), _) => {
        val subtypeInputType = RetrieveType.retrieveInputTypeFromFunction(isMember, env)
        
        isTypeConvertible(subtypeInputType, endingType, env)
      }
      case _ => Success(false)
    }
  }

  def isFeatureSetConvertible(
    startingFeatureSet: MapFeatureSet,
    endingFeatureSet: MapFeatureSet
  ) = startingFeatureSet match {
    case BasicMap => true
    case SimpleFunction => (endingFeatureSet != BasicMap)
    case FullFunction => (endingFeatureSet == FullFunction)
  }

  // If this function only allows one input, then return the output for that input
  def outputIfFunctionHasSingularInput(nFunction: NewMapObject): Outcome[NewMapObject, String] = {
    nFunction match {
      case MapInstance(values, _) if (values.length == 1) => {
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

    val apparentTypeOfObject = RetrieveType(nObject, env)

    isTypeConvertible(apparentTypeOfObject, nSubtype, env) match {
      case Success(true) => {
        // Note: This is a shortcut, but not always true
        // - Can we redo isTypeConvertible so it acts purely as a shortcut (maybe a version that doesn't call isPureTypeConvertible)
        Success(true)
      }
      case _ => nSubtype match {
        case SubtypeT(isMember) => {
          for {
            result <- Evaluator.quickApplyFunctionAttempt(isMember, nObject, env)

            // Instead of calling RetrieveType on the result, look at the outputType on nSubtype
            //  and find a default on that!
            defaultValueOrResultType <- Evaluator.getDefaultValueOfCommandType(RetrieveType(result, env), env)
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