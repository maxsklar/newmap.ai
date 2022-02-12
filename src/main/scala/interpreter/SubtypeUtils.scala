package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Handles checking if two subtypes are comparable to one another
// and whether an object can be converted to a different type
object SubtypeUtils {
  def checkProposedObjectInSubtype(
    proposedObject: NewMapObject,
    expectedType: NewMapSubtype,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    for {
      isMember <- isMemberOfSubtype(proposedObject, expectedType, env)
      _ <- Outcome.failWhen(!isMember, s"Proposed object $proposedObject is not in subtype $expectedType")
    } yield proposedObject
  }

  // For ReqMaps, we need to ensure that all of the values are accounted for.
  // For Maps, we want to know that the default value is never used
  def doMapValuesCoverType(
    values: Vector[(NewMapObject, NewMapObject)],
    nType: NewMapSubtype
  ): Outcome[Boolean, String] = {
    val keys = values.map(_._1).toSet

    // This is the generic pattern, which means that everything will match
    // TODO: This is going to get more complicated with more patterns!!
    val genericPatternExists = keys.exists(key => key match {
      case ParameterObj(_, _) => true
      case _ => false
    })

    if (genericPatternExists) Success(true)
    else {
      for {
        keysToMatch <- enumerateAllValuesIfPossible(nType)
      } yield {
        (keysToMatch -- keys).isEmpty && (keys -- keysToMatch).isEmpty
      }
    }
  }

  def enumerateAllValuesIfPossible(nType: NewMapSubtype): Outcome[Set[NewMapObject], String] = {
    nType match {
      // TODO: What if values is too large? Should we make some restrictions here?
      // - Idea: have a value in the environment that gives us a maximum we are allowed to count up to
      case SubtypeT(MapInstance(values, _)) => Success(values.map(_._1).toSet)
      // TODO(2022): if simpleFunction is boolean function on a small finite parentType, we should be able to enumerate those
      // TODO(2022): this is also one of those advanced cases where we want to know if one set is a subset of another through some advanced argument (like monotonicity)
      case SubtypeT(RangeFunc(i)) => Success((0 until i.toInt).map(j => Index(j.toLong)).toSet)
      // TODO - structs and cases
      case _ => Failure(s"Can't enumerate the allowed values of $nType -- could be unimplemented")
    }
  }


  // Try to convert an object to having a new type
  // - Fail when the conversion is not possible
  // - If the object already has that type, then just return itself
  def attemptToConvertToType(
    nObject: NewMapObject,
    requestedType: NewMapSubtype,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    val nType = RetrieveType(nObject)

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

  // TODO: This can be made to work in much broader circumstances
  // - For example, if the endingType excludes only a finite amount of objects, then we can check
  //    to make sure that all of those are not in startingType
  def isTypeConvertible(
    startingType: NewMapSubtype,
    endingType: NewMapSubtype,
    env: Environment
  ): Outcome[Boolean, String] = {
    for {
      pureTypeConvertible <- isObjectConvertibleToType(
        ParameterObj("_", startingType), // TODO - this is an awkward solution! (and it's also wrong.. remove!!)
        RetrieveType.getParentType(endingType),
        env
      )

      _ <- Outcome.failWhen(!pureTypeConvertible, s"Non-convertible pure types from $startingType to $endingType")
    } yield {
      val doesEndtypeCoverParentType = endingType match {
        case SubtypeT(MapInstance(values, MapT(inputType, _, CommandOutput, BasicMap))) => {
          // TODO: Extend this to see if pattering matching in basic function covers the full type
          // Check that the function doesn't return the default value for any input
          doMapValuesCoverType(values, inputType).toOption.getOrElse(false)
        }
        case SubtypeT(LambdaInstance(_, _)) => {
          //TODO - we can check to determine if this is always true
          false
        }
        case SubtypeT(IsCommandFunc) => false
        case SubtypeT(RangeFunc(_)) => false
        case _ => {
          // TODO: Is this appropriate - shouldn't it be false
          true
        }
      }

      doesEndtypeCoverParentType || {
        // End type does not cover parent type
        // So, let's go through all the values of starting type (if we can) and see if we can brute force it
        val allConvertedOutcome = for {
          allValues <- enumerateAllValuesIfPossible(startingType)
          doAllConvert <- allMembersOfSubtype(allValues.toVector, endingType, env)
        } yield doAllConvert

        allConvertedOutcome.toOption.getOrElse(false)
      }
    }
  }

  // TODO: ultimately, more potential conversions will be added to the environment, making this function more interesting
  // ALSO: this is for automatic conversion. There should be another conversion, which is a superset of this, which is less automatic
  //  - To be used in cases where you want the programmer to specifically ask for a conversion!
  def isObjectConvertibleToType(
    startingObject: NewMapObject,
    endingType: NewMapSubtype,
    env: Environment
  ): Outcome[Boolean, String] = {
    val startingType = RetrieveType(startingObject)

    (startingType, endingType) match {
      case _ if (startingType == endingType) => Success(true)
      case (_, AnyT) => Success(true)
      case (_, SubtypeT(isMember)) => {
        
        val subtypeInputType = RetrieveType.retrieveInputTypeFromFunction(isMember)

        for {
          // 1: Check if startingObject is convertible to the inputType of isMember
          convertedObject <- attemptToConvertToType(startingObject, subtypeInputType, env)

          // If not evaluated, we can't check for membership
          // TODO: What if this is a complex function?
          // - Solution: only evaluate it if its a simplefunction..
          //   if it's a complex function, leave it alone, but don't compile, and have a good error
          //   message about it saying that we can't check for the subtype because we won't run your
          //   function (which could be some crazy infinite loop or fibonacci crap)
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
        val isFeatureSetConvertible = startingFeatureSet match {
          case BasicMap => true
          case SimpleFunction => (endingFeatureSet != BasicMap)
          case FullFunction => (endingFeatureSet == FullFunction)
        }

        // TODO: We may be able to convert between different completeness types, but add this as needed
        val isMapCompletenessConvertible = (startingCompleteness == endingCompleteness)

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
          isFeatureSetConvertible &&
          isMapCompletenessConvertible
        )
      }
      case(
        StructT(startingParams),
        StructT(endingParams)
      ) => {
        isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunction(startingParams),
          RetrieveType.retrieveInputTypeFromFunction(endingParams),
          env
        )
        // TODO: The outputs have to agree as well
      }
      case (StructT(mi@MapInstance(values, _)), _) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularOutputT <- Evaluator.convertObjectToType(singularOutput, env)
          isConvertible <- isTypeConvertible(singularOutputT, endingType, env)
        } yield isConvertible
      }
      case (_, StructT(mi@MapInstance(values, _))) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularOutputT <- Evaluator.convertObjectToType(singularOutput, env)
          isConvertible <- isObjectConvertibleToType(startingObject, singularOutputT, env)
        } yield isConvertible
      }
      case (CaseT(startingCases), CaseT(endingCases)) => {
        // Note the contravariance (ending cases first)
        // This is because a case class with fewer cases can be converted into one with more
        isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunction(endingCases),
          RetrieveType.retrieveInputTypeFromFunction(startingCases),
          env
        )
        // TODO: The outputs have to agree as well
      }
      case (CaseT(mi@MapInstance(values, _)), _) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularOutputT <- Evaluator.convertObjectToType(singularOutput, env)
          isConvertible <- isTypeConvertible(singularOutputT, endingType, env)
        } yield isConvertible
      }
      case (_, CaseT(mi@MapInstance(values, _))) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularOutputT <- Evaluator.convertObjectToType(singularOutput, env)
          isConvertible <- isObjectConvertibleToType(startingObject, singularOutputT, env)
        } yield isConvertible
      }
      case (SubtypeT(isMember), _) => {
        val subtypeInputType = RetrieveType.retrieveInputTypeFromFunction(isMember)
        
        isTypeConvertible(subtypeInputType, endingType, env)
      }
      case _ => Success(false)
    }
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
    nSubtype: NewMapSubtype,
    env: Environment
  ): Outcome[Boolean, String] = {
    // TODO: if nObject is a parameter, we may still be able to confirm that it's in the subtype if the
    // type of it is convertible
    // TODO: We should distinguish between a closed literal - for which we can call quickApplyFunctionAttempt - and non-closed literals

    val apparentTypeOfObject = RetrieveType(nObject)

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
            defaultValueOrResultType <- Evaluator.getDefaultValueOfCommandType(RetrieveType(result), env)
          } yield (result != defaultValueOrResultType)
        }
        case (nType: NewMapType) => {
          Success(true)
        }
      }
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
          result <- if (isIt) allMembersOfSubtype(restOfObjects, nSubtype, env) else Success(false)
        } yield result
      }
      case _ => Success(true)
    }
  }
}