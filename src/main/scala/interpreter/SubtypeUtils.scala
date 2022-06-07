package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Handles checking if two subtypes are comparable to one another
// and whether an object can be converted to a different type
object SubtypeUtils {
  // For ReqMaps, we need to ensure that all of the values are accounted for.
  // For Maps, we want to know that the default value is never used
  def doPatternsCoverType(
    keys: Vector[UntaggedObject],
    nType: UntaggedObject,
    env: Environment
  ): Outcome[Boolean, String] = {
    val nTypeOutcome = TypeChecker.getFinalUnderlyingType(nType, env, env.typeSystem.currentState)

    // This is the generic pattern, which means that everything will match
    // TODO: This is going to get more complicated with more patterns!!
    // - In the future, we want to know if the keys as a group have all the patterns to cover the type
    val wildcardPatternExists = keys.exists(k => isCatchallPattern(k, nType, env))

    if (wildcardPatternExists) {
      Success(true)
    }
    else {
      val piecemealCompletenessOutcome = nTypeOutcome match {
        case Success(CaseT(cases, _, _)) => Success(checkCaseComplete(keys, cases, env))
        /*case UCase(constructor, inputPattern) => {
          Evaluator.stripVersioningU(constructor, env) match {
            case UParametrizedCaseT(parameters, caseT) => {
              // 1) use inputPattern and parameters to update the encironment
              // 2) call checkCaseComplete(keys, caseT, newEnv)

              val newEnv = env.newParams(parameters)
              // patternMatchResult: Success(Map(T -> WildcardPattern(T)))
              // Problem is that the environment is going to contain a wildcard??

              //throw new Exception(s"doPatternsCoverType: $inputPattern -- $parameters -- $caseT\n --- $parametersAsPattern\n -- $patternMatchResult")

              val ccc = checkCaseComplete(keys, caseT.cases, newEnv)


              // keys: Vector(UCase(None,StructPattern(Vector())), UCase(Some,WildcardPattern(t)))
              // parameters: Vector((T,Type))
              // caseT: Case(Some: T~pi, None: Struct ())
              // inputPattern:  WildcardPattern(T)
              //throw new Exception(s"in case pattern: $keys -- $parameters -- $caseT -- $inputPattern\n --\n -- $ccc")
              Failure("Unimplemented UParametrizedCaseT reqmap")
            }
            case _ => Success(false)
          }
        }*/
        case Success(StructT(params, _, _, _)) => checkStructComplete(keys, params, env)
        /*case UType(CustomT(name, inputPattern)) => {
          val typeSystem = env.typeSystem
          val currentState = typeSystem.currentState

          for {
            currentMapping <- Outcome(typeSystem.historicalMapping.get(currentState), s"Current type mapping $currentState not found")
            currentTypeId <- Outcome(currentMapping.get(name), s"$name must be defined")
            currentUnderlyingType <- Outcome(typeSystem.typeToUnderlyingType.get(currentTypeId), s"Couldn't find underlying type for $name")

            currentParameterPattern = currentUnderlyingType._1
            currentUnderlyingExp = currentUnderlyingType._2

            underlyingT <- typeSystem.convertToNewMapType(currentUnderlyingExp)

            // use currentUnderlyingExp instead!
            // Also update env to take the parameters into account!
            result <- doPatternsCoverType(keys, UType(underlyingT), env)
          } yield result
        }*/
        case _ => Success(false)
      }

      piecemealCompletenessOutcome match {
        case Success(true) => Success(true)
        case _ => {
          val patternsMap = keys.map(key => key -> ObjectExpression(UIndex(1)))

          for {
            keysToMatch <- enumerateAllValuesIfPossible(nType, env)
          } yield {
            keysToMatch.forall(uKey => {
              Evaluator.attemptPatternMatchInOrder(patternsMap, uKey, env).isSuccess
            })
          }
        }
      }
    }
  }

  def checkCaseComplete(
    keys: Vector[UntaggedObject],
    cases: Vector[(UntaggedObject, NewMapExpression)],
    env: Environment
  ): Boolean = {
    // For each case key, we want to make sure that this case key is completely covered
    val constructors = cases.map(_._1)

    var returnVal = true

    for {
      constructor <- constructors
    } yield {
      
      // Look at our keys, and find the ones that are only for this case key, and save those patterns
      val patternsWithThisConstructor = keys.flatMap(key => key match {
        case UCase(untaggedConstructor, pattern) => Some(pattern)
        case _ => None
      })

      // Find the input type for this constructor, and make sure that all of THOSE inputs are accounted for
      // TODO - cleanup this pattern matching!!!
      Evaluator.attemptPatternMatchInOrder(cases, constructor, env) match {
        case Success(inputTypeExpression) => {
          //println(s"$constructor --- $inputTypeExpression -- $returnVal")
          val inputType = Evaluator(inputTypeExpression, env).toOption.get
          // TODO - we need to be able to convert an expression to a pattern
          // - Some of the parameters in the expression with map to patterns instead of objects
          // - Whole new Evaluator line!!

          //println(s"inputType: $inputType\n --- ${Evaluator.asType(inputType, env)}")

          Evaluator.asType(inputType, env) match {
            case Failure(_) => returnVal = false
            case Success(inputTypeT) => {
              doPatternsCoverType(patternsWithThisConstructor, UType(inputTypeT), env) match {
                case Success(false) | Failure(_) => returnVal = false
                case _ => ()
              }
            }
          }
        }
        case _ => returnVal = false
      }
    }

    returnVal
  }

  def checkStructComplete(
    keys: Vector[UntaggedObject],
    params: Vector[(UntaggedObject, NewMapExpression)],
    env: Environment
  ): Outcome[Boolean, String] = {
    // TODO: Implement
    Success(false)
  }

  // Returns true if the object is a pattern that will match everything in the type
  // nType is a pattern that represents a type
  def isCatchallPattern(pattern: UntaggedObject, nType: UntaggedObject, env: Environment): Boolean = {
    pattern match {
      case UWildcardPattern(_) => true
      case UStruct(patterns)  => {
        nType match {
          // TODO: In the future, maybe we can relax "basicMap" by matching other patterns
          // - That would require isCatchallPattern to match an nType that's a UntaggedObject, not just a NewMapObject
          case UType(StructT(params, _, _, _)) if (params.length == patterns.length) => {
            (patterns, params.map(_._2)).zipped.toVector.forall(x => {
              Evaluator(x._2, env).toOption.map(nObject => {
                isCatchallPattern(x._1, UType(Evaluator.asType(nObject, env).toOption.get), env)
              }).getOrElse(false) // We're not really set up for this yet!
            })
          }
          case _ => false 
        }
      }
      case _ => false
    }
  }

  def enumerateAllValuesIfPossible(nType: UntaggedObject, env: Environment): Outcome[Set[UntaggedObject], String] = {
    TypeChecker.getFinalUnderlyingType(nType, env, env.typeSystem.currentState) match {
      // TODO: What if values is too large? Should we make some restrictions here?
      // - Idea: have a value in the environment that gives us a maximum we are allowed to count up to
      case Success(SubtypeT(UMap(values), parentType, _)) => {
        // TODO - remove this case!
        enumerateMapKeys(values.map(_._1))
      }
      /*case UType(CaseT(values, parentType, BasicMap)) => {
        ???
      }*/
      case Success(IndexT(i)) => {
        Success((0 until i.toInt).map(j => UIndex(j.toLong)).toSet)
      }
      case Success(BooleanT) => {
        Success(Vector(UIndex(0), UIndex(1)).toSet)
      }
      case Success(undertype) => {
        throw new Exception(s"Can't enumerate the allowed values of $nType with underlying Type $undertype -- could be unimplemented")
        Failure(s"Can't enumerate the allowed values of $nType with underlying Type $undertype -- could be unimplemented")
      }
      case Failure(f) => Failure(f)
    }
  }

  def enumerateMapKeys(values: Vector[UntaggedObject]): Outcome[Set[UntaggedObject], String] = {
    values match {
      case value +: additionalValues => {
        if (RetrieveType.isTermPatternFree(value)) {
          for {
            addlValues <- enumerateMapKeys(additionalValues)
          } yield addlValues + value
        } else {
          Failure(s"Found non-ObjectPattern: $value")
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
  ): Outcome[Vector[UntaggedObject], String] = {
    (startingType, endingType) match {
      case _ if (startingType == endingType) => Success(Vector.empty)
      case (SubtypeT(isMember, parentType, featureSet), _) => {
        for {
          convertInstructions <- isTypeConvertible(parentType, endingType, env)
        } yield convertInstructions
      }
      case (_, SubtypeT(isMember, parentType, featureSet)) => {
        Failure(s"A) Starting Obj: $startingType\nStartingType: $startingType\nEndingType: $endingType")
      }
      case (
        MapT(startingTypeTransform, MapConfig(startingCompleteness, startingFeatureSet, _)),
        MapT(endingTypeTransform, MapConfig(endingCompleteness, endingFeatureSet, _))
      ) => {
        // TODO: This is not entirely true
        // I think we can convert these (so long as the feature set is compatible) - but conversion from
        //  CommandOutput might require adding a default pattern
        val isMapCompletenessConvertible = true

        for {
          _ <- Outcome.failWhen(startingTypeTransform.length != 1, s"map conversions only available for type transforms of length 1. StartingTypeTransform was $startingTypeTransform")
          _ <- Outcome.failWhen(endingTypeTransform.length != 1, s"map conversions only available for type transforms of length 1. endingTypeTransform was $endingTypeTransform")

          startingTypePair <- startingTypeTransform.head match {
            case (inputT, ObjectExpression(outputT)) => {
              for {
                inT <- Evaluator.asType(inputT, env)
                ouT <- Evaluator.asType(outputT, env)
              } yield (inT, ouT)
            }
            case _ => {
              Failure(s"Not implement for generic type transform: $startingTypeTransform")
            }
          }

          endingTypePair <- endingTypeTransform.head match {
            case (inputT, ObjectExpression(outputT)) => {
              for {
                inT <- Evaluator.asType(inputT, env)
                ouT <- Evaluator.asType(outputT, env)
              } yield (inT, ouT)
            }
            case _ => {
              Failure(s"Not implement for generic type transform: $endingTypeTransform")
            }
          }

          (startingInputType, startingOutputType) = startingTypePair
          (endingInputType, endingOutputType) = endingTypePair

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
      case (CustomT(name1, _), CustomT(name2, _)) if (name1 == name2) => {
        // There is where we use the type system to eventually to a graph search
        Success(Vector.empty)
      }
      /*case (ConstructedType(VersionedObjectLink(key1), params1), ConstructedType(VersionedObjectLink(key2), params2)) => {
        // Todo - we need to work out how this works
        // This is also where the covariance/contravariance information should come in!!
        // We are also not taking versions into account!!!
        // Eventually type constructions above (map, case) will be subsumed under this
        if ((key1.uuid == key2.uuid) && (params1 == params2)) {
          Success(Vector.empty)
        } else {
          Failure(s"Cannot convert constructed types: $startingType to $endingType")
        }
      }*/
      case (HistoricalTypeT(uuid), TypeT) if (uuid == env.typeSystem.currentState) => {
        // I feel like more needs to be done here
        // TypeT requires the uuid as a case?
        // - Perhaps that's our next step
        Success(Vector.empty)
      }
      case (TypeT, HistoricalTypeT(uuid)) if (uuid == env.typeSystem.currentState) => {
        Success(Vector.empty)
      }
      case (WithStateT(typeSystemId1, CustomT(name1, params1)), WithStateT(typeSystemId2, CustomT(name2, params2))) => {
        for {
          typeId1 <- env.typeSystem.getTypeIdFromName(typeSystemId1, name1)
          typeId2 <- env.typeSystem.getTypeIdFromName(typeSystemId2, name2)

          _ <- Outcome.failWhen(params1 != params2, s"Params aren't equal: $params1 --- $params2")

          conversionRules <- env.typeSystem.searchForConvertibility(typeId1, typeId2)
        } yield {
          conversionRules
        }
      }
      case (WithStateT(typeSystemId1, CustomT(name1, params1)), CustomT(name2, params2)) => {
        for {
          typeId1 <- env.typeSystem.getTypeIdFromName(typeSystemId1, name1)
          typeId2 <- env.typeSystem.getTypeIdFromName(env.typeSystem.currentState, name2)

          _ <- Outcome.failWhen(params1 != params2, s"Params aren't equal: $params1 --- $params2")

          conversionRules <- env.typeSystem.searchForConvertibility(typeId1, typeId2)
        } yield {
          conversionRules
        }
      }
      case (CustomT(name1, params1), WithStateT(typeSystemId2, CustomT(name2, params2))) => {
        for {
          typeId1 <- env.typeSystem.getTypeIdFromName(env.typeSystem.currentState, name1)
          typeId2 <- env.typeSystem.getTypeIdFromName(typeSystemId2, name2)
          
          _ <- Outcome.failWhen(params1 != params2, s"Params aren't equal: $params1 --- $params2")

          conversionRules <- env.typeSystem.searchForConvertibility(typeId1, typeId2)
        } yield {
          conversionRules
        }
      }
      case (WithStateT(typeSystemId, CustomT(name, params)), _) => {
        for {
          underlyingStartingType <- TypeChecker.getFinalUnderlyingType(env.typeSystem.typeToUntaggedObject(startingType), env, typeSystemId)
          
          _ <- underlyingStartingType match {
            case SubtypeT(_, _, _) => Success()
            case _ => {
              throw new Exception(s"underlying type is only directly convertible on subtype - instead was $underlyingStartingType to $endingType")
              Failure(s"underlying type is only directly convertible on subtype - instead was $underlyingStartingType to $endingType")
            }
          }

          conversionRules <- isTypeConvertible(underlyingStartingType, endingType, env)
        } yield {
          conversionRules
        }
      }
      case _ => {
        startingType match {
          case WildcardPatternT(_) => throw new Exception(s"No rule to convert $startingType to $endingType")
          case _ => ()
        }
        Failure(s"No rule to convert $startingType to $endingType")
      }
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
  ): Outcome[Vector[UntaggedObject], String] = {
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
      case WildcardPatternT(_) => {
        Success(Vector.empty)
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
  def outputIfFunctionHasSingularInput(mapValues: Vector[(UntaggedObject, NewMapExpression)]): Outcome[NewMapExpression, String] = {
    if (mapValues.length == 1) {
      Success(mapValues.head._2)
    } else {
      Failure("Function did not have singular input")
    }
  }
}