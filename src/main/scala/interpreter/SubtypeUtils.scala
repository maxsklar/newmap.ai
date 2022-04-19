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

    val objectKeys: Set[UntaggedObject] = keys.flatMap(_ match {
      case ObjectPattern(o) => Some(o)
      case _ => None
    }).toSet

    // This is the generic pattern, which means that everything will match
    // TODO: This is going to get more complicated with more patterns!!
    // - In the future, we want to know if the keys as a group have all the patterns to cover the type
    val WildcardPatternExists = keys.exists(k => isCatchallPattern(k, nType, env))

    if (WildcardPatternExists) {
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
        untaggedConstructor <- constructors
      } yield {
        val taggedConstructor = TaggedObject(untaggedConstructor, caseKeys)

        // Look at our keys, and find the ones that are only for this case key, and save those patterns
        val patternsWithThisConstructor = keys.flatMap(key => key match {
          case CasePattern(untaggedConstructor, pattern) => Some(pattern)
          case ObjectPattern(UCase(untaggedConstructor, input)) => Some(ObjectPattern(input))
          case _ => None
        })

        // Find the input type for this constructor, and make sure that all of THOSE inputs are accounted for
        Evaluator.applyFunctionAttempt(cases, taggedConstructor, env) match {
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
  def isCatchallPattern(pattern: NewMapPattern, nType: NewMapObject, env: Environment): Boolean = {
    pattern match {
      case ObjectPattern(_) => false
      case WildcardPattern(_) => true
      case StructPattern(patterns)  => {
        nType match {
          // TODO: In the future, maybe we can relax "basicMap" by matching other patterns
          // - That would require isCatchallPattern to match an nType that's a NewMapPattern, not just a NewMapObject
          case StructT(TaggedObject(UMap(params), MapT(_, _, MapConfig(_, BasicMap, _, _)))) if (params.length == patterns.length) => {
            (patterns, params.map(_._2)).zipped.toVector.forall(x => {
              Evaluator(x._2, env).toOption.map(nObject => {
                isCatchallPattern(x._1, nObject, env)
              }).getOrElse(false) // We're not really set up for this yet!
            })
          }
          case _ => false 
        }
      }
      case CasePattern(_, _) => false
    }
  }

  def enumerateAllValuesIfPossible(nType: NewMapObject, env: Environment): Outcome[Set[UntaggedObject], String] = {
    Evaluator.stripVersioning(nType, env) match {
      // TODO: What if values is too large? Should we make some restrictions here?
      // - Idea: have a value in the environment that gives us a maximum we are allowed to count up to
      case SubtypeT(TaggedObject(UMap(values), _)) => {
        enumerateMapKeys(values.map(_._1))
      }
      case TaggedObject(UIndex(i), _) => {
        Success((0 until i.toInt).map(j => UIndex(j.toLong)).toSet)
      }
      case OrBooleanT => {
        Success(Vector(UIndex(0), UIndex(1)).toSet)
      }
      case _ => Failure(s"Can't enumerate the allowed values of $nType -- could be unimplemented")
    }
  }

  def enumerateMapKeys(values: Vector[NewMapPattern]): Outcome[Set[UntaggedObject], String] = {
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

  def doesTypeCoverParentType(nType: NewMapObject, env: Environment): Boolean = nType match {
    case SubtypeT(TaggedObject(untaggedFunction, MapT(inputType, _, MapConfig(CommandOutput, _, _, _)))) => {
      untaggedFunction match {
        case UMap(values) => {
          // TODO: Extend this to see if pattering matching in basic function covers the full type
          // Check that the function doesn't return the default value for any input
          doPatternsCoverType(values.map(_._1), inputType, env).toOption.getOrElse(false)
        }
        case IsCommandFunc | IsSimpleFunction => false
        case _ => true
      }
    }
    case _ => {
      // TODO: Is this appropriate - shouldn't it be false
      true
    }
  }

  def doesTypeCoverSubtype(startingType: NewMapObject, endingType: NewMapObject, env: Environment): Boolean = {
    val doesEndtypeCoverParentType = doesTypeCoverParentType(endingType, env)
    val doesStarttypeCoverParentType = doesTypeCoverParentType(startingType, env)

    endingType match {
      case _ if doesEndtypeCoverParentType => true
      case SubtypeT(TaggedObject(IsSimpleFunction, _)) => {
        startingType match {
          case MapT(_, _, config) => isFeatureSetConvertible(config.featureSet, SimpleFunction)
          case _ => false 
        }
      }
      case SubtypeT(TaggedObject(IsCommandFunc, _)) => {
        // This is definitely wrong, but we can fix when we move this out
        (startingType == endingType)
      }
      case _ => (!doesStarttypeCoverParentType && {
        // End type does not cover parent type
        // So, let's go through all the values of starting type (if we can) and see if we can brute force it
        val allConvertedOutcome = for {
          allValues <- enumerateAllValuesIfPossible(startingType, env)

          taggedAllValues = allValues.toVector.map(u => TaggedObject(u, startingType))

          doAllConvert <- allMembersOfSubtype(taggedAllValues, endingType, env)
        } yield doAllConvert

        allConvertedOutcome.toOption.getOrElse(false)
      })
    }
  }

  // Return - Instructions in the form of functions for converting from one type to another
  def isTypeConvertible(
    startingType: NewMapObject,
    endingType: NewMapObject,
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = {
    (startingType, endingType) match {
      case _ if (startingType == endingType) => Success(Vector.empty)
      case (_, AnyT) => Success(Vector.empty)
      case (CountT, TypeT) => Success(Vector.empty)
      case (ExpandingSubsetT(_), TypeT) => Success(Vector.empty)
      case (TaggedObject(_, ExpandingSubsetT(parentType)), _) => {
        for {
          convertInstructions <- isTypeConvertible(parentType, endingType, env)
        } yield convertInstructions
      }
      //case (TableT(_, _), TypeT) => Success(Vector.empty) // TODO: Do we need this? We might!
      case (_, SubtypeT(isMember)) => {
        val superType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(isMember), env)

        for {
          convertInstructions <- isTypeConvertible(startingType, superType, env)
          // TODO - starting type must be converted before moving on

          doesCover = doesTypeCoverSubtype(startingType, SubtypeT(isMember), env)
          _ <- Outcome.failWhen(!doesCover, s"Cannot convert: $startingType doesn't cover $endingType")
        } yield convertInstructions
      }
      case (_, TaggedObject(_, ExpandingSubsetT(_))) => {
        Failure(s"A) Starting Obj: $startingType\nStartingType: $startingType\nEndingType: $endingType")
      }
      case (
        MapT(startingInputType, startingOutputType, MapConfig(startingCompleteness, startingFeatureSet, _, _)),
        MapT(endingInputType, endingOutputType, MapConfig(endingCompleteness, endingFeatureSet, _, _))
      ) => {
        // TODO: This is not entirely true
        // I think we can convert these (so long as the feature set is compatible) - but conversion from
        //  CommandOutput might require adding a default pattern
        val isMapCompletenessConvertible = true

        for {
          // Note: the input type is CONTRAvariant, the output type is COvariant, hence the reversal
          // Eventually, we'll have to implement covariance in generic types

          // PROBLEM IS THAT isTypeConvertible doesn't then check if the types line up WRT subsets
          inputTypesConversionInstruction <- isTypeConvertible(endingInputType, startingInputType, env)

          outputTypesConversionInstructions <- isTypeConvertible(startingOutputType, endingOutputType, env)

          _ <- Outcome.failWhen(
            !isFeatureSetConvertible(startingFeatureSet, endingFeatureSet),
            s"Feature sets not convertible in maps $startingType -- $endingType"
          )

          _ <- Outcome.failWhen(
            !isMapCompletenessConvertible,
            s"Completeness not convertible in maps $startingType -- $endingType"
          )

        } yield {
          // TODO - utilizing the input/output type converstion instructions
          Vector.empty
        }
      }
      case(StructT(startingParams), StructT(endingParams)) => {
        isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(startingParams), env),
          RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(endingParams), env),
          env
        )
      }
      case (StructT(mi@TaggedObject(UMap(values), _)), _) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularObj <- Evaluator(singularOutput, env)
          convertInstructions <- isTypeConvertible(singularObj, endingType, env)
        } yield convertInstructions
      }
      case (_, StructT(mi@TaggedObject(UMap(values), _))) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularObj <- Evaluator(singularOutput, env)
          convertInstructions <- isTypeConvertible(startingType, singularObj, env)
        } yield convertInstructions
      }
      case (CaseT(startingCases), CaseT(endingCases)) => {
        // Note the contravariance (ending cases first)
        // This is because a case class with fewer cases can be converted into one with more
        for {
          convertInstructions <- isTypeConvertible(
            RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(endingCases), env),
            RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(startingCases), env),
            env
          )
        } yield convertInstructions
        // TODO: The outputs have to agree as well
      }
      case (CaseT(mi@TaggedObject(UMap(values), _)), _) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularObj <- Evaluator(singularOutput, env)
          convertInstructions <- isTypeConvertible(singularObj, endingType, env)
        } yield convertInstructions
      }
      case (_, CaseT(mi@TaggedObject(UMap(values), _))) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(mi)
          singularObj <- Evaluator(singularOutput, env)
          convertInstructions <- isTypeConvertible(startingType, singularObj, env)
        } yield convertInstructions
      }
      case (SubtypeT(isMember), _) => {
        val subtypeInputType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(isMember), env)
        
        for {
          convertInstructions <- isTypeConvertible(subtypeInputType, endingType, env)
        } yield convertInstructions
      }
      case (
        VersionedObjectLink(VersionedObjectKey(versionNumber, uuid), status),
        VersionedObjectLink(VersionedObjectKey(versionNumber2, uuid2), status2)
      ) if (uuid == uuid2) && (status2 == KeepUpToDate) => {
        // Take advantage of the fact that types are backwards compatible
        Success(Vector.empty)
      }
      case (
        VersionedObjectLink(VersionedObjectKey(versionNumber, uuid), status),
        _
      ) => {
        Evaluator.stripVersioning(startingType, env) match {
          case TaggedObject(UMap(_), ExpandingSubsetT(parentType)) => {
            isTypeConvertible(parentType, endingType, env)
          }
          case _ => {
            Failure(s"B) StartingType: $startingType\nEndingType: $endingType")
          }
        }
      }
      case (
        _,
        VersionedObjectLink(VersionedObjectKey(versionNumber, uuid), status)
      ) => {
        Failure(s"C) StartingType: $startingType\nEndingType: $endingType")
      }
      case _ => Failure(s"No rule to convert $startingType to $endingType")
    }
  }

  // TODO: ultimately, more potential conversions will be added to the environment, making this function more interesting
  // ALSO: this is for automatic conversion. There should be another conversion, which is a superset of this, which is less automatic
  //  - To be used in cases where you want the programmer to specifically ask for a conversion!
  // @return a list of simple function if the conversion requires application in order to convert
  def isObjectConvertibleToType(
    startingObject: NewMapObject,
    endingType: NewMapObject,
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = endingType match {
    case SubtypeT(isMember) => {
      val superType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(isMember), env)
      for {
        convertInstructions <- isObjectConvertibleToType(startingObject, superType, env)

        // TODO - make explicit conversion
        convertedObject = startingObject

        checksMembership <- isMemberOfSubtype(startingObject, isMember, env)
        _ <- Outcome.failWhen(!checksMembership, "Couldn't confirm member of subtype: $startingObject, $endingType")
      } yield {
        convertInstructions
      }
    }
    case _ => {
      val nType = RetrieveType.fromNewMapObject(startingObject, env)
      isTypeConvertible(nType, endingType, env)
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

  // Returns true if nObject is a member of a SubtypeT by its isMember Function
  // - assuming that it's already a member of the parent type
  def isMemberOfSubtype(
    nObject: NewMapObject,
    isMember: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    for {
      result <- Evaluator.applyFunctionAttempt(isMember, nObject, env)

      // Instead of calling RetrieveType on the result, look at the outputType on nSubtype
      //  and find a default on that!
      defaultValueOrResultType <- Evaluator.getDefaultValueOfCommandType(RetrieveType.fromNewMapObject(result, env), env)
    } yield (result != defaultValueOrResultType)
  }

  def allMembersOfSubtype(
    nObjects: Vector[NewMapObject],
    nSubtype: NewMapObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    nObjects match {
      case nObject +: restOfUObjects => {
        for {
          isIt <- isMemberOfSubtype(nObject, nSubtype, env)
          result <- if (isIt) allMembersOfSubtype(restOfUObjects, nSubtype, env) else Success(false)
        } yield result
      }
      case _ => Success(true)
    }
  }
}