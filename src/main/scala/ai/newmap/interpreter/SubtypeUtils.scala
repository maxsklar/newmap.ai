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
    nType: NewMapType,
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
        case Success(CaseT(UMap(cases), _, _)) => Success(checkCaseComplete(keys, cases, env))
        case Success(StructT(UMap(params), _, _, _)) => checkStructComplete(keys, params, env)
        case _ => Success(false)
      }

      piecemealCompletenessOutcome match {
        case Success(true) => Success(true)
        case _ => {
          val patternsMap = keys.map(key => key -> UIndex(1))

          for {
            keysToMatch <- IterationUtils.enumerateAllValuesIfPossible(nType, env)
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
    cases: Vector[(UntaggedObject, UntaggedObject)],
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
        case UCase(untaggedConstructor, pattern) => {
          if (untaggedConstructor == constructor) Some(pattern) else None
        }
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
              doPatternsCoverType(patternsWithThisConstructor, inputTypeT, env) match {
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
    params: Vector[(UntaggedObject, UntaggedObject)],
    env: Environment
  ): Outcome[Boolean, String] = {
    // TODO: There is surely a better, more comprehensive algorithm for this, but
    //  we'll have something here for now to attempt to solve this.

    // TODO: Implement
    Success(false)
  }

  // Returns true if the object is a pattern that will match everything in the type
  // nType is a pattern that represents a type
  def isCatchallPattern(pattern: UntaggedObject, nType: NewMapType, env: Environment): Boolean = {
    pattern match {
      case UWildcardPattern(_) => true
      case UStruct(patterns)  => {
        nType match {
          // TODO: In the future, maybe we can relax "basicMap" by matching other patterns
          // - That would require isCatchallPattern to match an nType that's a UntaggedObject, not just a NewMapObject
          case StructT(UMap(params), _, _, _) if (params.length == patterns.length) => {
            (patterns, params.map(_._2)).zipped.toVector.forall(x => {
              Evaluator(x._2, env).toOption.map(nObject => {
                isCatchallPattern(x._1, Evaluator.asType(nObject, env).toOption.get, env)
              }).getOrElse(false) // We're not really set up for this yet!
            })
          }
          case _ => false 
        }
      }
      case _ => false
    }
  }

  case class IsTypeConvertibleResponse(
    convertInstructions: Vector[FunctionWithMatchingRules],
    refinedEndingType: NewMapType,
    newParameters: Map[String, UntaggedObject] = Map.empty
  )

  // Return - Instructions in the form of functions for converting from one type to another
  def isTypeConvertible(
    startingType: NewMapType,
    endingType: NewMapType,
    env: Environment
  ): Outcome[IsTypeConvertibleResponse, String] = {
    //println(s"Calling isTypeConvertible: $startingType -- $endingType")
    val emptyResponse = IsTypeConvertibleResponse(Vector.empty, endingType)

    (startingType, endingType) match {
      case (startingType, WildcardPatternT(name)) => {
        Success(IsTypeConvertibleResponse(
          Vector.empty,
          startingType,
          Map(name -> env.typeSystem.typeToUntaggedObject(startingType))
        ))
      }
      case (CustomT(name1, param1), CustomT(name2, param2)) if (name1 == name2) => {
        //println(s"Found my way here: $name1 -- $param1 --- $name2 --- $param2")
        for {
          parameterType <- env.typeSystem.getParameterType(env.typeSystem.currentState, name1)

          // TODO: eventually, there should be a general way to convert a type to a matcher
          matcher = if (isTypeConvertible(parameterType, TypeT, env).isSuccess) TypeMatcher else StandardMatcher

          newParameters <- Evaluator.attemptPatternMatch(param2, param1, matcher, env)
        } yield {
          val refinedEndingType = endingType // TODO - recheck this!

          IsTypeConvertibleResponse(Vector.empty, refinedEndingType, newParameters)
        }
      }
      case _ if (startingType == endingType) => Success(emptyResponse)
      case (SubtypeT(isMember, parentType, featureSet), _) => {
        isTypeConvertible(parentType, endingType, env)
      }
      case (_, SubtypeT(isMember, parentType, featureSet)) => {
        Failure(s"A) Starting Obj: $startingType\nStartingType: $startingType\nEndingType: $endingType")
      }
      case (
        MapT(startingTypeTransform, MapConfig(startingCompleteness, startingFeatureSet, _, _, _)),
        MapT(endingTypeTransform, MapConfig(endingCompleteness, endingFeatureSet, _, _, _))
      ) => {
        // TODO: This is not entirely true
        // I think we can convert these (so long as the feature set is compatible) - but conversion from
        //  CommandOutput might require adding a default pattern
        val isMapCompletenessConvertible = true

        for {
          startingTypePair <- startingTypeTransform match {
            case UMap(Vector((inputT, outputT))) => {
              for {
                inT <- Evaluator.asType(inputT, env)
                ouT <- Evaluator.asType(outputT, env)
              } yield (inT, ouT)
            }
            case UMapPattern(inputT, outputT) => {
              for {
                inT <- Evaluator.asType(inputT, env)
                ouT <- Evaluator.asType(outputT, env)
              } yield (inT, ouT)
            }
            case _ => {
              Failure(s"Not implement for generic type transform: $startingTypeTransform")
            }
          }

          endingTypePair <- endingTypeTransform match {
            case UMap(Vector((inputT, outputT)))  => {
              for {
                inT <- Evaluator.asType(inputT, env)
                ouT <- Evaluator.asType(outputT, env)
              } yield (inT, ouT)
            }
            case UMapPattern(inputT, outputT) => {
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
          emptyResponse
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
      case (StructT(values, fieldParentType, _, _), _) if (isSingularMap(values)) => {
        for {
          singularOutput <- outputIfFunctionHasSingularInput(values, fieldParentType, env)
          singularObj <- Evaluator(singularOutput, env)
          singularObjT <- Evaluator.asType(singularObj, env)
          response <- isTypeConvertible(singularObjT, endingType, env)
        } yield response
      }
      case (_, StructT(values, fieldParentType, _, _)) => {
        for {
          valueBindings <- values.getMapBindings()
          singularOutputResponse <- convertToStructWithSingleValue(valueBindings, env)
          singularObj <- Evaluator(singularOutputResponse.inputType, env)
          singularObjT <- Evaluator.asType(singularObj, env)
          response <- isTypeConvertible(startingType, singularObjT, env)
        } yield response.copy(convertInstructions = singularOutputResponse.conversionRules ++ response.convertInstructions)
      }
      case (CaseT(startingCases, startingFieldType, _), CaseT(endingCases, endingFieldType, _)) => {
        // Note the contravariance (ending cases first)
        // This is because a case class with fewer cases can be converted into one with more
        /*for {
          response <- isTypeConvertible(
            RetrieveType.retrieveInputTypeFromFunctionObj(endingCases, env),
            RetrieveType.retrieveInputTypeFromFunctionObj(startingCases, env),
            env
          )
        } yield response*/
        // TODO: The outputs have to agree as well
        Failure("Need to implement case conversion")
      }
      case (CaseT(values, startingFieldType, _), _) if (isSingularMap(values)) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(values, startingFieldType, env)
          singularObj <- Evaluator(singularOutput, env)
          singularObjT <- Evaluator.asType(singularObj, env)
          response <- isTypeConvertible(singularObjT, endingType, env)
        } yield response
      }
      case (WithStateT(typeSystemId, CustomT(_, _)), CaseT(values, endingFieldType, _)) if (isSingularMap(values)) => {
        // TODO: It's confusing why we would need this, but it prevents the case directly below from firing
        // -- Eventually this whole method should be refactored.
        for {
          underlyingStartingType <- TypeChecker.getFinalUnderlyingType(startingType, env, typeSystemId)
          response <- isTypeConvertible(underlyingStartingType, endingType, env)
        } yield {
          response
        }
      }
      case (_, CaseT(values, endingFieldType, _)) if (isSingularMap(values)) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(values, endingFieldType, env)
          singularObj <- Evaluator(singularOutput, env)
          singularObjT <- Evaluator.asType(singularObj, env)
          response <- isTypeConvertible(startingType, singularObjT, env)
        } yield response
      }
      case (HistoricalTypeT(uuid), TypeT) if (uuid == env.typeSystem.currentState) => {
        // I feel like more needs to be done here
        // TypeT requires the uuid as a case?
        // - Perhaps that's our next step
        Success(emptyResponse)
      }
      case (TypeT, HistoricalTypeT(uuid)) if (uuid == env.typeSystem.currentState) => {
        Success(emptyResponse)
      }
      case (WithStateT(typeSystemId1, CustomT(name1, params1)), WithStateT(typeSystemId2, CustomT(name2, params2))) => {
        for {
          typeId1 <- env.typeSystem.getTypeIdFromName(typeSystemId1, name1)
          typeId2 <- env.typeSystem.getTypeIdFromName(typeSystemId2, name2)

          _ <- Evaluator.attemptStandardPatternMatch(params1, params2, env)

          convertInstructions <- env.typeSystem.searchForConvertibility(typeId1, typeId2)
        } yield {
          val refinedEndingType = endingType // TODO - recheck this!

          IsTypeConvertibleResponse(convertInstructions, refinedEndingType)
        }
      }
      case (WithStateT(typeSystemId1, CustomT(name1, params1)), CustomT(name2, params2)) => {
        for {
          typeId1 <- env.typeSystem.getTypeIdFromName(typeSystemId1, name1)
          typeId2 <- env.typeSystem.getTypeIdFromName(env.typeSystem.currentState, name2)

          _ <- Evaluator.attemptStandardPatternMatch(params1, params2, env)

          convertInstructions <- env.typeSystem.searchForConvertibility(typeId1, typeId2)
        } yield {
          val refinedEndingType = endingType // TODO - recheck this!

          IsTypeConvertibleResponse(convertInstructions, refinedEndingType)
        }
      }
      case (CustomT(name1, params1), WithStateT(typeSystemId2, CustomT(name2, params2))) => {
        for {
          typeId1 <- env.typeSystem.getTypeIdFromName(env.typeSystem.currentState, name1)
          typeId2 <- env.typeSystem.getTypeIdFromName(typeSystemId2, name2)
          
          _ <- Evaluator.attemptStandardPatternMatch(params1, params2, env)

          convertInstructions <- env.typeSystem.searchForConvertibility(typeId1, typeId2)
        } yield {
          val refinedEndingType = endingType // TODO - recheck this!

          IsTypeConvertibleResponse(convertInstructions, refinedEndingType)
        }
      }
      case (WithStateT(typeSystemId, CustomT(name, params)), _) => {
        for {
          underlyingStartingType <- TypeChecker.getFinalUnderlyingType(startingType, env, typeSystemId)
          
          _ <- underlyingStartingType match {
            case SubtypeT(_, _, _) => Success(())
            case CustomT(_, _) => Success(())
            case _ => {
              //throw new Exception(s"underlying type is only directly convertible on subtype - instead was $underlyingStartingType to $endingType")
              Failure(s"underlying type is only directly convertible on subtype - instead was $underlyingStartingType to $endingType")
            }
          }

          response <- isTypeConvertible(underlyingStartingType, endingType, env)
        } yield {
          response
        }
      }

      case (CountT, DoubleT) => {
        Success(
          IsTypeConvertibleResponse(
            Vector(FunctionWithMatchingRules(UCountToDecimal, StandardMatcher)),
            DoubleT
          ),
        )
      }
      case _ => {
        Failure(s"No rule to convert ${startingType.displayString(env)} to ${endingType.displayString(env)}")
      }
    }
  }

  // TODO: this is for automatic conversion. There should be another conversion, which is a superset of this, which is less automatic
  //  - To be used in cases where you want the programmer to specifically ask for a conversion!
  // @return if the object is convertible, return the tagged object that represents its data in the new type
  def attemptConvertObjectToType(
    startingObject: NewMapObject,
    endingType: NewMapType,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    endingType match {
      case SubtypeT(isMember, parentType, featureSet) => {
        for {
          tObject <- attemptConvertObjectToType(startingObject, parentType, env)
          membershipCheck <- Evaluator.applyFunctionAttempt(isMember, tObject.uObject, env)
          _ <- Outcome.failWhen(membershipCheck == UInit, s"Not member of subtype: $startingObject, $endingType")
        } yield {
          tObject
        }
      }
      case _ => {
        for {
          response <- isTypeConvertible(startingObject.nType, endingType, env)

          convertedStartingObject <- Evaluator.applyListOfFunctions(
            startingObject.uObject,
            response.convertInstructions,
            env
          )
        } yield NewMapObject(convertedStartingObject, response.refinedEndingType)
      }
    }
  }

  // I'm pretty sure that this is can be simplified with an ordering!
  def isFeatureSetConvertible(
    startingFeatureSet: MapFeatureSet,
    endingFeatureSet: MapFeatureSet
  ) = {
    startingFeatureSet.getLevel <= endingFeatureSet.getLevel
  }

  def isSingularMap(uObject: UntaggedObject): Boolean = uObject match {
    case UMap(values) if (values.length == 1) => true
    case UMapPattern(_, _) => true
    case _ => false
  }

  // If this function only allows one input, then return the output for that input
  def outputIfFunctionHasSingularInput(
    mapValues: UntaggedObject,
    fieldType: NewMapType,
    env: Environment
  ): Outcome[UntaggedObject, String] = mapValues match {
    case UMap(Vector(head)) => {
      // THIS IS WHERE WE NEED TO DEAL WITH PATTERNS IN THE MAP VALUE
      val key = head._1

      for {
        newParameters <- RetrieveType.fetchParamsFromPattern(fieldType, key, env)
        _ <- Outcome.failWhen(newParameters.size > 0, "Function had a pattern")
      } yield head._2
    }
    case UMapPattern(key, value) => {
      for {
        newParameters <- RetrieveType.fetchParamsFromPattern(fieldType, key, env)
        _ <- Outcome.failWhen(newParameters.size > 0, "Function had a pattern")
      } yield value
    }
    case _ => Failure("Function did not have singular input")
  }

  case class StructWithSingleValueResponse(
    inputType: UntaggedObject,
    conversionRules: Vector[FunctionWithMatchingRules]
  )

  // If this is a struct with a single (non-defaulted) value, then figure out that type, and have a conversion function
  def convertToStructWithSingleValue(
    mapValues: Vector[(UntaggedObject, UntaggedObject)],
    env: Environment
  ): Outcome[StructWithSingleValueResponse, String] = {
    if (mapValues.length >= 1) {
      // In this case, make sure that all the other values have a default
      for {
        defaultStruct <- CommandMaps.getDefaultValueFromStructParams(mapValues.tail, env)
      } yield {
        // How do I convert this?

        val conversionRule = UMap(Vector(
          UWildcardPattern("item") ->
            UMap(Vector(mapValues.head._1 -> ParamId("item")) ++ defaultStruct)
        ))

        // The "standardMatcher" always works here because we only have a Wildcard Pattern
        // - And all matchers treat the wildcard pattern the same way
        val matcher = StandardMatcher

        StructWithSingleValueResponse(mapValues.head._2, Vector(FunctionWithMatchingRules(conversionRule, matcher)))
      }
    } else {
      Failure("Function did not have singular input")
    }
  }
}