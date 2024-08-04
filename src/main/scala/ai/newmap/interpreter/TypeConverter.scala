  package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

case class TypeConvertionResponse(
  convertInstructions: Vector[FunctionWithMatchingRules],
  refinedEndingType: NewMapType,
  newParameters: Map[String, UntaggedObject] = Map.empty
)

/**
 * Handles checking if two subtypes are comparable to one another
 */
object TypeConverter {
  // Return - Instructions in the form of functions for converting from one type to another
  def isTypeConvertible(
    startingType: NewMapType,
    endingType: NewMapType,
    env: Environment
  ): Outcome[TypeConvertionResponse, String] = {
    //println(s"Calling isTypeConvertible: $startingType -- $endingType")
    val emptyResponse = TypeConvertionResponse(Vector.empty, endingType)

    (startingType, endingType) match {
      case (startingType, WildcardT(name)) => {
        Success(TypeConvertionResponse(
          Vector.empty,
          startingType,
          Map(name -> startingType.asUntagged)
        ))
      }
      // Do we need this case?
      case (CustomT(name1, param1, typeSystemId1), CustomT(name2, param2, typeSystemId2)) if (name1 == name2 && typeSystemId1 == typeSystemId2) => {
        for {
          parameterType <- env.typeSystem.getParameterType(env.typeSystem.currentVersion, name1)
          matcher = matcherForType(parameterType, env) 
          newParameters <- Evaluator.patternMatch(param2, param1, matcher, env)
        } yield {
          val refinedEndingType = endingType // TODO - recheck this!

          TypeConvertionResponse(Vector.empty, refinedEndingType, newParameters)
        }
      }
      case (ArrayT(param1), ArrayT(param2)) => {
        // TODO - this is really a copy of the customT situation above
        for {
          newParameters <- Evaluator.patternMatch(param2.asUntagged, param1.asUntagged, TypeMatcher, env)
        } yield {
          val refinedEndingType = endingType // TODO - recheck this!

          TypeConvertionResponse(Vector.empty, refinedEndingType, newParameters)
        }
      }
      case _ if (startingType == endingType) => Success(emptyResponse)
      case (SubtypeT(_, parentType, _), _) => {
        isTypeConvertible(parentType, endingType, env)
      }
      case (_, SubtypeT(_, _, _)) => {
        Failure(s"A) Starting Obj: $startingType\nStartingType: $startingType\nEndingType: $endingType")
      }
      case (
        MapT(TypeTransform(startingInputType, startingOutputType), MapConfig(startingCompleteness, startingFeatureSet, _, _, _)),
        MapT(TypeTransform(endingInputType, endingOutputType), MapConfig(endingCompleteness, endingFeatureSet, _, _, _))
      ) => {
        //println("We are here: " + startingType + " -- " + endingType)

        // TODO: This is not entirely true
        // I think we can convert these (so long as the feature set is compatible) - but conversion from
        //  CommandOutput might require adding a default pattern
        val isMapCompletenessConvertible = (startingCompleteness != PartialMap) && (startingCompleteness != PartialMap)

        for {
          // Note: the input type is CONTRAvariant, the output type is COvariant, hence the reversal
          // Eventually, we'll have to implement covariance in generic types

          // PROBLEM IS THAT isTypeConvertible doesn't then check if the types line up WRT subsets
          //TODO - change TypeTransform, so that we know what type of pattern the key is!
          // Create a UExternalWildcard - it's rare, and can only exist in these type transforms
          inputTypesConversionInstruction <- isTypeConvertible(endingInputType, startingInputType, env)

          outputTypesConversionInstructions <- isTypeConvertible(startingOutputType, endingOutputType, env)

          _ <- Outcome.failWhen(
            !isFeatureSetConvertible(startingFeatureSet, endingFeatureSet),
            s"Feature sets not convertible in maps $startingType -- $endingType"
          )

          _ <- Outcome.failWhen(
            !isMapCompletenessConvertible,
            s"Completeness not convertible in maps $startingCompleteness -- $endingCompleteness"
          )

        } yield {
          // TODO - utilizing the input/output type converstion instructions
          emptyResponse
        }
      }
      case(StructT(_, _, _, _), StructT(_, _, _, _)) => {
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
          singularObjT <- singularObj.asType
          response <- isTypeConvertible(singularObjT, endingType, env)
        } yield response
      }
      case (_, StructT(values, _, _, _)) => {
        for {
          valueBindings <- values.getMapBindings()
          singularOutputResponse <- convertToStructWithSingleValue(valueBindings, env)
          singularObj <- Evaluator(singularOutputResponse.inputType, env)
          singularObjT <- singularObj.asType
          response <- isTypeConvertible(startingType, singularObjT, env)
        } yield {
          response.copy(convertInstructions = singularOutputResponse.conversionRules ++ response.convertInstructions)
        }
      }
      case (CaseT(_, _, _), CaseT(_, _, _)) => {
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
          singularObjT <- singularObj.asType
          response <- isTypeConvertible(singularObjT, endingType, env)
        } yield response
      }
      case (CustomT(name1, params1, typeSystemId1), CustomT(name2, params2, typeSystemId2)) if (name1 == name2) => {
        for {
          _ <- Evaluator.standardPatternMatch(params1, params2, env)

          convertInstructions <- env.typeSystem.searchForForwardConvertibility(name1, typeSystemId1, typeSystemId2)
        } yield {
          TypeConvertionResponse(convertInstructions, endingType)
        }
      }
      case (CustomT(name, params, typeSystemId), _) => {
        for {
          underlyingT <- TypeChecker.getUnderlyingType(name, params, env, typeSystemId)
          response <- isTypeConvertible(underlyingT, endingType, env)
        } yield response
      }
      case (_, CaseT(values, endingFieldType, _)) if (isSingularMap(values)) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        for {
          singularOutput <- outputIfFunctionHasSingularInput(values, endingFieldType, env)
          singularObj <- Evaluator(singularOutput, env)
          singularObjT <- singularObj.asType
          response <- isTypeConvertible(startingType, singularObjT, env)
        } yield response
      }
      case (startingType, endingType) => {
        for {
          (nType, conversionResponse) <- env.typeSystem.convertibilityGraph.findPotentialConversions(startingType)
        } {
          for {
            response <- isTypeConvertible(nType, endingType, env)
          } {
            // TODO - figure out the real way to compose conversion responses
            val newConvertInstructions = conversionResponse.convertInstructions ++ response.convertInstructions
            return Success(response.copy(convertInstructions = newConvertInstructions))
          }
        }
        // See if we can convert any of these further to the ending type

        val str = s"No rule to convert ${startingType} to ${endingType} -- ${startingType == endingType}"
        Failure(str)
      }
    }
  }

  def matcherForType(nType: NewMapType, env: Environment): MatchingRules = {
    // TODO: eventually, there should be a general way to convert a type to a matcher
    if (isTypeConvertible(nType, TypeT, env).isSuccess) TypeMatcher else StandardMatcher
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
      case SubtypeT(isMember, parentType, _) => {
        for {
          tObject <- attemptConvertObjectToType(startingObject, parentType, env)
          membershipCheck <- Evaluator.applyFunction(isMember, tObject.uObject, env)
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
    case UArray(values) if (values.length == 1) => true
    case USingularMap(_, _) => true
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
    case USingularMap(key, value) => {
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
      val conversionRule = UMap(Vector(
        UWildcard("item") ->
          UMap(Vector(mapValues.head._1 -> ParamId("item")))
      ))

      // The "standardMatcher" always works here because we only have a Wildcard Pattern
      // - And all matchers treat the wildcard pattern the same way
      val matcher = StandardMatcher

      Success(StructWithSingleValueResponse(mapValues.head._2, Vector(FunctionWithMatchingRules(conversionRule, matcher))))
    } else {
      Failure("Function did not have singular input")
    }
  }
}