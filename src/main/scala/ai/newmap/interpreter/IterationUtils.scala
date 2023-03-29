package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Provides libraries for iterating type and object
// - to be replaced by internal newmap.ai code
object IterationUtils {
  def enumerateAllValuesIfPossible(nType: NewMapType, env: Environment): Outcome[Vector[UntaggedObject], String] = {
    TypeChecker.getFinalUnderlyingType(nType, env, env.typeSystem.currentState) match {
      // TODO: What if values is too large? Should we make some restrictions here?
      // - Idea: have a value in the environment that gives us a maximum we are allowed to count up to
      case Success(SubtypeT(UMap(values), parentType, _)) => {
        // TODO - remove this case!
        enumerateMapKeys(values.map(_._1))
      }
      case Success(CaseT(cases, CustomT("String", UStruct(v)), BasicMap)) if (v.isEmpty) => {
        for {
          paramList <- StatementInterpreter.convertMapValuesToParamList(cases, env)
          result <- enumerateCaseValues(paramList, env)
        } yield result
      }
      case Success(IndexT(UIndex(i))) => {
        Success((0 until i.toInt).map(j => UIndex(j.toLong)).toVector)
      }
      case Success(BooleanT) => {
        Success(Vector(UIndex(0), UIndex(1)))
      }
      case Success(StructT(params, parentFieldType, RequireCompleteness, BasicMap)) => {
        // This is dangerous because the multiplications can cause a large blowout of allowed types
        
        params match {
          case (name, firstParamType) +: otherParams => {
            for {
              firstParamT <- env.typeSystem.convertToNewMapType(firstParamType)
              firstParamValues <- enumerateAllValuesIfPossible(firstParamT, env)
              otherParamValues <- enumerateAllValuesIfPossible(StructT(otherParams, parentFieldType, RequireCompleteness, BasicMap), env)
            } yield {
              val valueList = for {
                firstParam <- firstParamValues
                otherParam <- otherParamValues

                otherParamValues <- otherParam match {
                  case UStruct(values) => Some(values)
                  case _ => None
                }
              } yield {
                firstParam +: otherParamValues
              }

              valueList.map(v => UStruct(v))
            }
          }
          case _ => {
            Success(Vector(UStruct(Vector.empty)))
          }
        }
        /*
          case class StructT(
            params: Vector[(UntaggedObject, UntaggedObject)],
            fieldParentType: NewMapType,
            completeness: MapCompleteness = RequireCompleteness,
            featureSet: MapFeatureSet = BasicMap
          ) extends NewMapType

        */
      }
      case Success(undertype) => {
        //throw new Exception(s"Can't enumerate the allowed values of $nType with underlying Type $undertype -- could be unimplemented")
        Failure(s"Can't enumerate the allowed values of $nType with underlying Type $undertype -- could be unimplemented")
      }
      case Failure(f) => Failure(f)
    }
  }

  def enumerateMapKeys(values: Vector[UntaggedObject]): Outcome[Vector[UntaggedObject], String] = {
    values match {
      case value +: additionalValues => {
        if (RetrieveType.isTermPatternFree(value)) {
          for {
            addlValues <- enumerateMapKeys(additionalValues)
          } yield value +: addlValues
        } else {
          Failure(s"Found non-ObjectPattern: $value")
        }
      }
      case _ => Success(Vector.empty) 
    }
  }

  def enumerateCaseValues(
    cases: Vector[(String, NewMapType)],
    env: Environment
  ): Outcome[Vector[UntaggedObject], String] = {
    cases match {
      case firstCase +: otherCases => {
        for {
          firstCaseValues <- enumerateAllValuesIfPossible(firstCase._2, env)
          otherCaseValues <- enumerateCaseValues(otherCases, env)
        } yield {
          val taggedFirstCaseVals = firstCaseValues.map(v => UCase(UIdentifier(firstCase._1), v))
          taggedFirstCaseVals ++ otherCaseValues
        }
      }
      case _ => Success(Vector.empty)
    }
  }

  def iterateObject(
    nObject: NewMapObject,
    env: Environment,
    typeSystemIdOpt: Option[UUID] = None
  ): Outcome[Vector[UntaggedObject], String] = {
    Evaluator.stripVersioning(nObject, env) match {
      case TaggedObject(UMap(values), MapT(_, MapConfig(CommandOutput, BasicMap, _, _, _))) => {
        Success(values.map(_._1))
      }
      case TaggedObject(UMap(values), MapT(_, MapConfig(RequireCompleteness, BasicMap, _, _, _))) => {
        Success(values.map(_._2))
      }
      case TaggedObject(UStruct(values), MapT(_, MapConfig(CommandOutput, BasicMap, _, _, _))) => {
        Success(values)
      }
      case TaggedObject(UStruct(values), MapT(UMap(typeTransform), MapConfig(RequireCompleteness, BasicMap, _, _, _))) => {
        for {
          ttv <- typeTransformValues(typeTransform, env.typeSystem)
          enumeration <- enumerateAllValuesIfPossible(ttv.keyType, env)
        } yield enumeration
      }
      case TaggedObject(UCase(UIndex(i), UStruct(values)), CustomT("Array", itemType)) => {
        Success(values)
      }
      case TaggedObject(UCase(UIndex(i), UMap(values)), CustomT("Array", itemType)) => {
        // We should be more careful about the ordering here!!
        Success(values.map(_._2))
      }
      case TaggedObject(untaggedCurrent, CustomT(typeName, params)) => {
        val typeSystemId = typeSystemIdOpt.getOrElse(env.typeSystem.currentState)
        val typeSystemMapping = env.typeSystem.historicalMapping.get(typeSystemId).getOrElse(Map.empty) 

        for {
          typeId <- Outcome(typeSystemMapping.get(typeName), s"Couldn't find type: $typeName")

          underlyingTypeInfo <- Outcome(env.typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find type: $typeName -- $typeId")

          (underlyingPattern, underlyingExp) = underlyingTypeInfo

          patternMatchSubstitutions <- Evaluator.attemptPatternMatch(underlyingPattern, params, StandardMatcher, env)

          underlyingType = MakeSubstitution(underlyingExp, patternMatchSubstitutions)

          underlyingT <- env.typeSystem.convertToNewMapType(underlyingType)

          currentResolved <- TypeChecker.tagAndNormalizeObject(untaggedCurrent, underlyingT, env)

          result <- iterateObject(currentResolved, env, typeSystemIdOpt)
        } yield result
      }
      case TaggedObject(untaggedCurrent, WithStateT(typeSystemId, nType)) => {
        for {
          retaggedCurrent <- TypeChecker.tagAndNormalizeObject(untaggedCurrent, nType, env)
          result <- iterateObject(retaggedCurrent, env, Some(typeSystemId))
        } yield result
      }
      case TaggedObject(untaggedCurrent, TypeT) => {
        for {
          nType <- env.typeSystem.convertToNewMapType(untaggedCurrent)
          result <- enumerateAllValuesIfPossible(nType, env)
        } yield result
      }
      case TaggedObject(untaggedCurrent, nType) => {
        Failure(s"Unable to iterate over object: $untaggedCurrent of type ${nType.displayString(env)}")
      }
      case _ => Failure(s"Unable to iterate over object: ${nObject.displayString(env)}")
    }
  }

  case class TypeTransformValuesResponse(
    keyType: NewMapType,
    valueType: NewMapType
  )

  def typeTransformValues(
    values: Vector[(UntaggedObject, UntaggedObject)],
    typeSystem: NewMapTypeSystem
  ): Outcome[TypeTransformValuesResponse, String] = {
    for {
      ttItem <- Outcome(values.headOption, "type transform has no values")
      _ <- Outcome.failWhen(values.length != 1, s"Can't yet handle typeTransforms without a single binding: $values")
      (keyType, valueType) = ttItem
      keyT <- typeSystem.convertToNewMapType(keyType)
      valueT <- typeSystem.convertToNewMapType(valueType)
    } yield {
      TypeTransformValuesResponse(keyT, valueT)
    }
  }

  // Give a type, what's going to come out of the iteration?
  def iterationItemType(nType: NewMapType, env: Environment, typeSystemIdOpt: Option[UUID] = None): Outcome[NewMapType, String] = {
    nType match {
      case MapT(UMap(typeTransform), MapConfig(CommandOutput, BasicMap, _, _, _)) => {
        for {
          ttv <- typeTransformValues(typeTransform, env.typeSystem)
        } yield {
          ttv.keyType
        }
      }
      case MapT(UMap(typeTransform), MapConfig(RequireCompleteness, BasicMap, _, _, _)) => {
        for {
          ttv <- typeTransformValues(typeTransform, env.typeSystem)
        } yield {
          ttv.valueType
        }
      }
      case CustomT("Array", itemType) => {
        env.typeSystem.convertToNewMapType(itemType)
      }
      case CustomT(typeName, params) => {
        val typeSystemId = typeSystemIdOpt.getOrElse(env.typeSystem.currentState)
        val typeSystemMapping = env.typeSystem.historicalMapping.get(typeSystemId).getOrElse(Map.empty) 

        for {
          typeId <- Outcome(typeSystemMapping.get(typeName), s"Couldn't find type: $typeName")
          underlyingTypeInfo <- Outcome(env.typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find type: $typeName -- $typeId")

          (underlyingPattern, underlyingExp) = underlyingTypeInfo

          patternMatchSubstitutions <- Evaluator.attemptPatternMatch(underlyingPattern, params, StandardMatcher, env)

          underlyingType = MakeSubstitution(underlyingExp, patternMatchSubstitutions)

          underlyingT <- env.typeSystem.convertToNewMapType(underlyingType)
          result <- iterationItemType(underlyingT, env, typeSystemIdOpt)
        } yield result
      }
      case WithStateT(typeSystemId, nType) => {
        iterationItemType(nType, env, Some(typeSystemId))
      }
      case _ => Failure(s"Unable to get iteration item type: ${nType.displayString(env)}")
    }
  }

  // Can I iterate the first type into the second type?
  // This means that the first type is either convertible into the second type, or it's iteration type can be convertible.
  // For example, if my second type is Int, then I can certainly give it an Int. But I can also give it an array of Int because I'm stil getting Ints out of it!
  def isIteratableToType(firstType: NewMapType, secondType: NewMapType, env: Environment): Outcome[Boolean, String] = {
    if (SubtypeUtils.isTypeConvertible(firstType, secondType, env).isSuccess) {
      Success(true)
    } else {
      for {
        firstIt <- iterationItemType(firstType, env)
        result <- isIteratableToType(firstIt, secondType, env)
      } yield result
    }
  }
}