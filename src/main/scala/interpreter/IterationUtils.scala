package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

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
      /*case CaseT(values, parentType, BasicMap) => {
        ???
      }*/
      case Success(IndexT(UIndex(i))) => {
        Success((0 until i.toInt).map(j => UIndex(j.toLong)).toVector)
      }
      case Success(BooleanT) => {
        Success(Vector(UIndex(0), UIndex(1)))
      }
      case Success(undertype) => {
        throw new Exception(s"Can't enumerate the allowed values of $nType with underlying Type $undertype -- could be unimplemented")
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

  def iterateObject(nObject: NewMapObject, env: Environment): Outcome[Vector[UntaggedObject], String] = {
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
      case _ => Failure(s"Unable to iterate over object: $nObject")
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
  def iterationItemType(nType: NewMapType, env: Environment): Outcome[NewMapType, String] = {
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
      case _ => Failure(s"Unable to get iteration item type: $nType")
    }
  }
}