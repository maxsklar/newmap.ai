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
      case Success(SubtypeT(UMap(values), _, _)) => {
        // TODO - remove this case!
        enumerateMapKeys(values.map(_._1))
      }
      case Success(CaseT(UMap(cases), CustomT("String", UStruct(v)), BasicMap)) if (v.isEmpty) => {
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
      case Success(StructT(UMap(params), parentFieldType, RequireCompleteness, BasicMap)) => {
        // This is dangerous because the multiplications can cause a large blowout of allowed types
        
        params match {
          case (_, firstParamType) +: otherParams => {
            for {
              firstParamT <- firstParamType.asType
              firstParamValues <- enumerateAllValuesIfPossible(firstParamT, env)
              otherParamValues <- enumerateAllValuesIfPossible(StructT(UMap(otherParams), parentFieldType, RequireCompleteness, BasicMap), env)
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
        if (isTermPatternFree(value)) {
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
    nObject match {
      case NewMapObject(UMap(values), MapT(_, MapConfig(CommandOutput, BasicMap, _, _, _))) => {
        Success(values.map(_._1))
      }
      case NewMapObject(UMap(values), MapT(_, MapConfig(RequireCompleteness, BasicMap, _, _, _))) => {
        Success(values.map(_._2))
      }
      case NewMapObject(UStruct(values), MapT(_, MapConfig(CommandOutput, BasicMap, _, _, _))) => {
        Success(values)
      }
      case NewMapObject(UStruct(_), MapT(typeTransform, MapConfig(RequireCompleteness, BasicMap, _, _, _))) => {
        enumerateAllValuesIfPossible(typeTransform.keyType, env)
      }
      case NewMapObject(UCase(UIndex(_), UStruct(values)), CustomT("Array", _)) => {
        Success(values)
      }
      case NewMapObject(UCase(UIndex(_), UMap(values)), CustomT("Array", _)) => {
        // We should be more careful about the ordering here!!
        Success(values.map(_._2))
      }
      case NewMapObject(untaggedCurrent, CustomT(typeName, params)) => {
        val typeSystemId = typeSystemIdOpt.getOrElse(env.typeSystem.currentState)
        val typeSystemMapping = env.typeSystem.historicalMapping.get(typeSystemId).getOrElse(Map.empty) 

        for {
          typeId <- Outcome(typeSystemMapping.get(typeName), s"Couldn't find type: $typeName")

          underlyingTypeInfo <- Outcome(env.typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find type: $typeName -- $typeId")

          (underlyingPattern, underlyingExp) = underlyingTypeInfo

          patternMatchSubstitutions <- Evaluator.patternMatch(underlyingPattern, params, StandardMatcher, env)

          underlyingType = MakeSubstitution(underlyingExp, patternMatchSubstitutions)

          underlyingT <- underlyingType.asType

          currentResolved <- TypeChecker.tagAndNormalizeObject(untaggedCurrent, underlyingT, env)

          result <- iterateObject(currentResolved, env, typeSystemIdOpt)
        } yield result
      }
      case NewMapObject(untaggedCurrent, WithStateT(typeSystemId, nType)) => {
        for {
          retaggedCurrent <- TypeChecker.tagAndNormalizeObject(untaggedCurrent, nType, env)
          result <- iterateObject(retaggedCurrent, env, Some(typeSystemId))
        } yield result
      }
      case NewMapObject(untaggedCurrent, TypeT) => {
        for {
          nType <- untaggedCurrent.asType
          result <- enumerateAllValuesIfPossible(nType, env)
        } yield result
      }
      case NewMapObject(uType, HistoricalTypeT(typeSystemId)) => {
        val newUType = UCase(
          UIdentifier("WithState"),
          UCase(
            Uuuid(typeSystemId),
            uType
          )
        )

        iterateObject(NewMapObject(newUType, TypeT), env)
      }
      case NewMapObject(untaggedCurrent, nType) => {
        Failure(s"Unable to iterate over object: $untaggedCurrent of type ${nType.displayString(env)}")
      }
      case _ => Failure(s"Unable to iterate over object: ${nObject.displayString(env)}")
    }
  }

  // Give a type, what's going to come out of the iteration?
  def iterationItemType(nType: NewMapType, env: Environment, typeSystemIdOpt: Option[UUID] = None): Outcome[NewMapType, String] = {
    nType match {
      case MapT(typeTransform, MapConfig(CommandOutput, BasicMap, _, _, _)) => {
        Success(typeTransform.keyType)
      }
      case MapT(typeTransform, MapConfig(RequireCompleteness, BasicMap, _, _, _)) => {
        Success(typeTransform.valueType)
      }
      case CustomT("Array", itemType) => {
        itemType.asType
      }
      case CustomT(typeName, params) => {
        val typeSystemId = typeSystemIdOpt.getOrElse(env.typeSystem.currentState)
        val typeSystemMapping = env.typeSystem.historicalMapping.get(typeSystemId).getOrElse(Map.empty) 

        for {
          typeId <- Outcome(typeSystemMapping.get(typeName), s"Couldn't find type: $typeName")
          underlyingTypeInfo <- Outcome(env.typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find type: $typeName -- $typeId")

          (underlyingPattern, underlyingExp) = underlyingTypeInfo

          patternMatchSubstitutions <- Evaluator.patternMatch(underlyingPattern, params, StandardMatcher, env)

          underlyingType = MakeSubstitution(underlyingExp, patternMatchSubstitutions)

          underlyingT <- underlyingType.asType
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
    if (TypeConversionCalculator.isTypeConvertible(firstType, secondType, env).isSuccess) {
      Success(true)
    } else {
      for {
        firstIt <- iterationItemType(firstType, env)
        result <- isIteratableToType(firstIt, secondType, env)
      } yield result
    }
  }

  def isTermPatternFree(uObject: UntaggedObject): Boolean = uObject match {
    case UWildcardPattern(_) => false
    case UCase(constructor, input) => isTermPatternFree(constructor) && isTermPatternFree(input)
    case UStruct(patterns) => patterns.forall(isTermPatternFree(_))
    case UMap(items) => items.map(_._2).forall(isTermPatternFree(_))
    case _ => true
  }
}