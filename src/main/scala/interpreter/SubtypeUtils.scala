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
    nType: NewMapPattern,
    env: Environment
  ): Outcome[Boolean, String] = {
    val objectKeys: Set[UntaggedObject] = keys.flatMap(_ match {
      case ObjectPattern(o) => Some(o)
      case _ => None
    }).toSet

    // This is the generic pattern, which means that everything will match
    // TODO: This is going to get more complicated with more patterns!!
    // - In the future, we want to know if the keys as a group have all the patterns to cover the type
    val wildcardPatternExists = keys.exists(k => isCatchallPattern(k, nType, env))

    if (wildcardPatternExists) {
      Success(true)
    }
    else {
      val piecemealCompletenessOutcome = nType match {
        case ObjectPattern(UType(CaseT(cases, _, _))) => checkCaseComplete(keys, cases, env)
        /*case UMap(values) => {
          checkCaseComplete(keys, UMap(values), env)
        }*/
        case ObjectPattern(UType(StructT(params, _, _, _))) => checkStructComplete(keys, params, env)
        case ObjectPattern(UType(CustomT(_, underlying))) => doPatternsCoverType(keys, ObjectPattern(UType(underlying)), env)
        /*case ULink(key) => {
          for {
            state <- Evaluator.indicatedState(key, env)
            untaggedState <- Evaluator.removeTypeTag(state)
            result <- doPatternsCoverType(keys, untaggedState, env)
          } yield result
        }*/ // TODO - add custom type here
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
    cases: Vector[(NewMapPattern, NewMapExpression)],
    env: Environment
  ): Outcome[Boolean, String] = {
    for {
      // For each case key, we want to make sure that this case key is completely covered
      constructors <- enumerateMapKeys(cases.map(_._1))
    } yield {
      var returnVal = true

      for {
        untaggedConstructor <- constructors
      } yield {
        // Look at our keys, and find the ones that are only for this case key, and save those patterns
        val patternsWithThisConstructor = keys.flatMap(key => key match {
          case CasePattern(untaggedConstructor, pattern) => Some(pattern)
          case ObjectPattern(UCase(untaggedConstructor, input)) => Some(ObjectPattern(input))
          case _ => None
        })

        // Find the input type for this constructor, and make sure that all of THOSE inputs are accounted for
        // TODO - cleanup this pattern matching!!!
        Evaluator.applyFunctionAttempt(UMap(cases), untaggedConstructor, env) match {
          case Success(inputType) => {
            Evaluator.asType(inputType, env) match {
              case Failure(_) => returnVal = false
              case Success(inputTypeT) => {
                doPatternsCoverType(patternsWithThisConstructor, ObjectPattern(UType(inputTypeT)), env) match {
                  case Success(false) | Failure(_) => returnVal = false
                  case _ => ()
                }
              }
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
    params: Vector[(NewMapPattern, NewMapExpression)],
    env: Environment
  ): Outcome[Boolean, String] = {
    // TODO: Implement
    Success(false)
  }

  // Returns true if the object is a pattern that will match everything in the type
  // nType is a pattern that represents a type
  def isCatchallPattern(pattern: NewMapPattern, nType: NewMapPattern, env: Environment): Boolean = {
    pattern match {
      case ObjectPattern(_) => false
      case WildcardPattern(_) => true
      case StructPattern(patterns)  => {
        nType match {
          // TODO: In the future, maybe we can relax "basicMap" by matching other patterns
          // - That would require isCatchallPattern to match an nType that's a NewMapPattern, not just a NewMapObject
          case ObjectPattern(UType(StructT(params, _, _, _))) if (params.length == patterns.length) => {
            (patterns, params.map(_._2)).zipped.toVector.forall(x => {
              Evaluator(x._2, env).toOption.map(nObject => {
                isCatchallPattern(x._1, ObjectPattern(UType(Evaluator.asType(nObject, env).toOption.get)), env)
              }).getOrElse(false) // We're not really set up for this yet!
            })
          }
          case _ => false 
        }
      }
      case CasePattern(_, _) => false
      case MapTPattern(_, _, _) => false
    }
  }

  def enumerateAllValuesIfPossible(nType: NewMapPattern, env: Environment): Outcome[Set[UntaggedObject], String] = {
    nType match {
      // TODO: What if values is too large? Should we make some restrictions here?
      // - Idea: have a value in the environment that gives us a maximum we are allowed to count up to
      case ObjectPattern(UType(SubtypeT(UMap(values), parentType, _))) => {
        // TODO - remove this case!
        enumerateMapKeys(values.map(_._1))
      }
      /*case ObjectPattern(UType(CaseT(values, parentType, BasicMap))) => {
        ???
      }*/
      case ObjectPattern(UType(IndexT(i))) => {
        Success((0 until i.toInt).map(j => UIndex(j.toLong)).toSet)
      }
      case ObjectPattern(UType(BooleanT)) => {
        Success(Vector(UIndex(0), UIndex(1)).toSet)
      }
      case _ => {
        throw new Exception(s"Can't enumerate the allowed values of $nType -- could be unimplemented")
        Failure(s"Can't enumerate the allowed values of $nType -- could be unimplemented")
      }
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

  // Return - Instructions in the form of functions for converting from one type to another
  def isTypeConvertible(
    startingType: NewMapType,
    endingType: NewMapType,
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = {
    (startingType, endingType) match {
      case _ if (startingType == endingType) => Success(Vector.empty)
      //case (_, AnyT) => Success(Vector.empty)
      //case (CountT, TypeT) => Success(Vector.empty)
      case (SubtypeT(isMember, parentType, featureSet), _) => {
        for {
          convertInstructions <- isTypeConvertible(parentType, endingType, env)
        } yield convertInstructions
      }
      case (_, SubtypeT(isMember, parentType, featureSet)) => {
        Failure(s"A) Starting Obj: $startingType\nStartingType: $startingType\nEndingType: $endingType")
      }
      case (
        MapT(startingInputType, startingOutputType, MapConfig(startingCompleteness, startingFeatureSet, _)),
        MapT(endingInputType, endingOutputType, MapConfig(endingCompleteness, endingFeatureSet, _))
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
      case(StructT(startingParams, startingFieldType, _, _), StructT(endingParams, endingFieldType, _, _)) => {
        // 1) Can we convert starting field type to ending field type?
        // - For each param in startingParams:
        //   - Convert to ending param
        //   - does the starting param type convert to ending param type?

        // TODO - rethink this!!
        /*isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunctionObj(startingParams, env),
          RetrieveType.retrieveInputTypeFromFunctionObj(endingParams, env),
          env
        )*/
        Failure("Need to implement struct conversion")
      }
      case (StructT(values, fieldParentType, _, _), _) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(values)
          singularObj <- Evaluator(singularOutput, env)
          singularObjT <- Evaluator.asType(singularObj, env)
          convertInstructions <- isTypeConvertible(singularObjT, endingType, env)
        } yield convertInstructions
      }
      case (_, StructT(values, fieldParentType, _, _)) if (values.length == 1) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(values)
          singularObj <- Evaluator(singularOutput, env)
          singularObjT <- Evaluator.asType(singularObj, env)
          convertInstructions <- isTypeConvertible(startingType, singularObjT, env)
        } yield convertInstructions
      }
      case (CaseT(startingCases, startingFieldType, _), CaseT(endingCases, endingFieldType, _)) => {
        // Note the contravariance (ending cases first)
        // This is because a case class with fewer cases can be converted into one with more
        /*for {
          convertInstructions <- isTypeConvertible(
            RetrieveType.retrieveInputTypeFromFunctionObj(endingCases, env),
            RetrieveType.retrieveInputTypeFromFunctionObj(startingCases, env),
            env
          )
        } yield convertInstructions*/
        // TODO: The outputs have to agree as well
        Failure("Need to implement case conversion")
      }
      case (CaseT(values, startingFieldType, _), _) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(values)
          singularObj <- Evaluator(singularOutput, env)
          singularObjT <- Evaluator.asType(singularObj, env)
          convertInstructions <- isTypeConvertible(singularObjT, endingType, env)
        } yield convertInstructions
      }
      case (_, CaseT(values, endingFieldType, _)) if (values.length == 1) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(values)
          singularObj <- Evaluator(singularOutput, env)
          singularObjT <- Evaluator.asType(singularObj, env)
          convertInstructions <- isTypeConvertible(startingType, singularObjT, env)
        } yield convertInstructions
      }
      case (CustomT(uuid, _), CustomT(uuid2, _)) if (uuid == uuid2) => {
        Success(Vector.empty)
      }
      case (ConstructedType(VersionedObjectLink(key1), params1), ConstructedType(VersionedObjectLink(key2), params2)) => {
        // Todo - we need to work out how this works
        // This is also where the covariance/contravariance information should come in!!
        // We are also not taking versions into account!!!
        // Eventually type constructions above (map, case) will be subsumed under this
        if ((key1.uuid == key2.uuid) && (params1 == params2)) {
          Success(Vector.empty)
        } else {
          Failure(s"Cannot convert constructed types: $startingType to $endingType")
        }
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
    endingType: NewMapType,
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = {
    endingType match {
      // What to do about this??? For some reason we can't put it in
      // Eventually - CustomT
      /*case VersionedObjectLink(key, _) => {
        for {
          currentEndingState <- Evaluator.currentState(key.uuid, env)
          result <- isObjectConvertibleToType(startingObject, currentEndingState, env)
        } yield result
      }*/
      case SubtypeT(isMember, parentType, featureSet) => {
        for {
          convertInstructions <- isObjectConvertibleToType(startingObject, parentType, env)

          // TODO - make explicit conversion
          convertedObject = startingObject

          soUntagged <- Evaluator.removeTypeTag(startingObject)
          membershipCheck <- Evaluator.applyFunctionAttempt(isMember, soUntagged, env)
          _ <- Outcome.failWhen(membershipCheck == UInit, s"Not member of subtype: $startingObject, $endingType")
        } yield {
          convertInstructions
        }
      }
      case _ => {
        val nType = RetrieveType.fromNewMapObject(startingObject, env)
        isTypeConvertible(nType, endingType, env)
      }
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
  def outputIfFunctionHasSingularInput(mapValues: Vector[(NewMapPattern, NewMapExpression)]): Outcome[NewMapExpression, String] = {
    if (mapValues.length == 1) {
      Success(mapValues.head._2)
    } else {
      Failure("Function did not have singular input")
    }
  }

  // In the future - replace this with a type class (implementing equals)
  def checkEqual(a: UntaggedObject, b: UntaggedObject): Boolean = {
    a == b
  }
}