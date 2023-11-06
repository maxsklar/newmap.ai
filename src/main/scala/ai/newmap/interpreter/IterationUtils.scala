package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Provides libraries for iterating type and object
// - to be replaced by internal newmap.ai code
object IterationUtils {
  def enumerateAllValuesIfPossible(nType: NewMapType, env: Environment): Outcome[Vector[UntaggedObject], String] = {
    TypeChecker.getFinalUnderlyingType(nType, env) match {
      // TODO: What if values is too large? Should we make some restrictions here?
      // - Idea: have a value in the environment that gives us a maximum we are allowed to count up to
      case Success(SubtypeT(UMap(values), _, _)) => {
        // TODO - remove this case!
        enumerateMapKeys(values.map(_._1))
      }
      case Success(CaseT(cases, CustomT("String", UArray(v), _), BasicMap)) if (v.isEmpty) => {
        for {
          caseMapBindings <- cases.getMapBindings
          paramList <- StatementInterpreter.convertMapValuesToParamList(caseMapBindings, env)
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
        
        for {
          mapBindings <- params.getMapBindings

          result <- mapBindings match {
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
                    case UArray(values) => Some(values)
                    case _ => None
                  }
                } yield {
                  firstParam +: otherParamValues
                }

                valueList.map(v => UArray(v))
              }
            }
            case _ => {
              Success(Vector(UArray()))
            }
          }
        } yield result
      }
      case Success(undertype) => {
        //throw new Exception(s"Can't enumerate the allowed values of $nType with underlying Type $undertype -- could be unimplemented")
        Failure(s"Can't enumerate the allowed values of ${nType.displayString(env)} with underlying Type $undertype -- could be unimplemented")
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
      case NewMapObject(UArray(values), MapT(_, MapConfig(CommandOutput, BasicMap, _, _, _))) => {
        Success(values.toVector)
      }
      case NewMapObject(UArray(_), MapT(typeTransform, MapConfig(RequireCompleteness, BasicMap, _, _, _))) => {
        enumerateAllValuesIfPossible(typeTransform.keyType, env)
      }
      case NewMapObject(UCase(UIndex(_), values), ArrayT(_)) => {
        for {
          mapBindings <- values.getMapBindings()
        } yield mapBindings.map(_._2)
      }
      case NewMapObject(untaggedCurrent, CustomT(typeName, params, typeSystemId)) => {
        for {
          underlyingTypeInfo <- env.typeSystem.historicalUnderlyingType(typeName, typeSystemId)
          
          (underlyingPattern, underlyingExp) = underlyingTypeInfo

          patternMatchSubstitutions <- Evaluator.patternMatch(underlyingPattern, params, StandardMatcher, env)

          underlyingType = MakeSubstitution(underlyingExp.asUntagged, patternMatchSubstitutions)

          underlyingT <- underlyingType.asType

          //currentResolved <- TypeChecker.tagAndNormalizeObject(untaggedCurrent, underlyingT, env)

          //result <- iterateObject(currentResolved, env, typeSystemIdOpt)
          result <- iterateObject(NewMapObject(untaggedCurrent, underlyingT), env, typeSystemIdOpt)
        } yield result
      }
      case NewMapObject(untaggedCurrent, TypeT) => {
        for {
          nType <- untaggedCurrent.asType
          result <- enumerateAllValuesIfPossible(nType, env)
        } yield result
      }
      case NewMapObject(untaggedCurrent, nType) => {
        Failure(s"Unable to iterate over object: $untaggedCurrent of type ${nType.displayString(env)}")
      }
      case _ => Failure(s"Unable to iterate over object: ${nObject.displayString(env)}")
    }
  }

  // Give a type, what's going to come out of the iteration?
  def iterationItemType(nType: NewMapType, env: Environment): Outcome[NewMapType, String] = {
    nType match {
      case MapT(typeTransform, MapConfig(CommandOutput, BasicMap, _, _, _)) => {
        Success(typeTransform.keyType)
      }
      case MapT(typeTransform, MapConfig(RequireCompleteness, BasicMap, _, _, _)) => {
        Success(typeTransform.valueType)
      }
      case ArrayT(itemT) => Success(itemT)
      case CustomT(typeName, params, typeSystemId) => {
        for {
          // TODO - use TypeChecker.getUnderlyingType
          underlyingTypeInfo <- env.typeSystem.historicalUnderlyingType(typeName, typeSystemId)

          (underlyingPattern, underlyingExp) = underlyingTypeInfo

          patternMatchSubstitutions <- Evaluator.patternMatch(underlyingPattern, params, StandardMatcher, env)

          underlyingType = MakeSubstitution(underlyingExp.asUntagged, patternMatchSubstitutions)

          underlyingT <- underlyingType.asType
          result <- iterationItemType(underlyingT, env)
        } yield result
      }
      case _ => Failure(s"Unable to get iteration item type: ${nType.displayString(env)}")
    }
  }

  // Can I iterate the first type into the second type?
  // This means that the first type is either convertible into the second type, or it's iteration type can be convertible.
  // For example, if my second type is Int, then I can certainly give it an Int. But I can also give it an array of Int because I'm stil getting Ints out of it!
  def isIteratableToType(firstType: NewMapType, secondType: NewMapType, env: Environment): Outcome[Boolean, String] = {
    if (TypeConverter.isTypeConvertible(firstType, secondType, env).isSuccess) {
      Success(true)
    } else {
      for {
        firstIt <- iterationItemType(firstType, env)
        result <- isIteratableToType(firstIt, secondType, env)
      } yield result
    }
  }

  def isTermPatternFree(uObject: UntaggedObject): Boolean = uObject match {
    case UWildcard(_) => false
    case UCase(constructor, input) => isTermPatternFree(constructor) && isTermPatternFree(input)
    case UArray(patterns) => patterns.forall(isTermPatternFree(_))
    case UMap(items) => items.map(_._2).forall(isTermPatternFree(_))
    case _ => true
  }
}