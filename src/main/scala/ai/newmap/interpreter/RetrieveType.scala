package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

/*
 * This object is now misnamed, since more stuff was added to it and its original type-retrieving capabilities have
 * become unneccesary
 * TODO: Either move these methods to different places, or rename this as a util object.
 */
object RetrieveType {
  def isTermPatternFree(uObject: UntaggedObject): Boolean = uObject match {
    case UWildcardPattern(_) => false
    case UCase(constructor, input) => isTermPatternFree(constructor) && isTermPatternFree(input)
    case UStruct(patterns) => patterns.forall(isTermPatternFree(_))
    case UMap(items) => items.map(_._2).forall(isTermPatternFree(_))
    case _ => true
  }

  // Ensures that there are no free variables in this term
  // TODO: See if this can be handled by a specialized type-checker on UntaggedObject (ripe for removing this code)
  // - But it's not that simple yet
  def isTermClosedLiteral(
    nExpression: UntaggedObject,
    knownVariables: Vector[String] = Vector.empty,
  ): Boolean = nExpression match {
    case ParamId(name) => knownVariables.contains(name)
    case ApplyFunction(func, input, _) => {
      isTermClosedLiteral(func, knownVariables) &&
        isTermClosedLiteral(input, knownVariables)
    }
    case UCase(_, input) => isTermClosedLiteral(input, knownVariables)
    case UMap(values) => {
      isMapValuesClosed(values, knownVariables)
    }
    case UStruct(values) => values.forall(v => isTermClosedLiteral(v, knownVariables))
    case constant => true
  }

  def isMapValuesClosed(
    mapValues: Vector[(UntaggedObject, UntaggedObject)],
    knownVariables: Vector[String]
  ): Boolean = {
    mapValues match {
      case (pattern, expression) +: restOfMapValues => {
        val newKnownVariables = Evaluator.newParametersFromPattern(pattern)
        isTermClosedLiteral(expression, knownVariables ++ newKnownVariables) &&
          isMapValuesClosed(restOfMapValues, knownVariables)
      }
      case _ => true
    }
  }

  def getParameterValues(
    identifier: String,
    env: Environment
  ): Outcome[Map[String, NewMapType], String] = {
    for {
      parameterType <- env.typeSystem.getParameterType(env.typeSystem.currentState, identifier)
      parameterPattern <- env.typeSystem.getParameterPattern(env.typeSystem.currentState, identifier)
      paramValues <- fetchParamsFromPattern(parameterType, parameterPattern, env)
    } yield paramValues
  }

  def fetchParamsFromPattern(
    nType: NewMapType,
    pattern: UntaggedObject,
    env: Environment
  ): Outcome[Map[String, NewMapType], String] = {
    val expectedTypeOutcome = TypeChecker.getFinalUnderlyingType(
      nType,
      env,
      env.typeSystem.currentState
    )

    for {
      expectedType <- expectedTypeOutcome

      answer <- (expectedType, pattern) match {
        case (_, UWildcardPattern(x)) => Success(Map(x -> nType))
        case (CaseT(cases, _, _), UCase(constructor, resultPattern)) => {
          for {
            valueTypeExpression <- Evaluator.applyFunctionAttempt(cases, constructor, env)
            valueType <- Evaluator(valueTypeExpression, env)
            valueT <- env.typeSystem.convertToNewMapType(valueType)
            result <- fetchParamsFromPattern(valueT, resultPattern, env)
          } yield result
        }
        case (StructT(UMap(params), _, _, BasicMap), UStruct(values)) => {
          if (params.length == values.length) {
            var resultMap: Map[String, NewMapType] = Map.empty

            for {
              i <- (0 until params.length)
            } {
              for {
                uType <- Evaluator(params(i)._2, env)
                nType <- env.typeSystem.convertToNewMapType(uType)
                patterns <- fetchParamsFromPattern(nType, values(i), env)
              } {
                resultMap = resultMap ++ patterns.toVector
              }
            }

            Success(resultMap)
          } else {
            Failure(s"params and values don't match in length: $params --- $values")
          }
        }
        case (StructT(params, _, _, BasicMap), UMap(values)) => {
          Failure("not implemented - fetchParamsFromPattern on StructT with Map pattern")
        }
        case (MapT(typeTransform, config), UMap(values)) => {
          Failure("not implemented - fetchParamsFromPattern on MapT")
        }
        case (SubtypeT(_, parentType, _), _) => fetchParamsFromPattern(parentType, pattern, env)
        case _ => {
          val resultMap: Map[String, NewMapType] = Map.empty
          Success(resultMap)
        }
      }
    } yield answer
  }

  def getMapBindings(
    map: UntaggedObject
  ): Outcome[Vector[(UntaggedObject, UntaggedObject)], String] = map match {
    case UMap(values) => Success(values)
    case UMapPattern(key, value) => Success(Vector(key -> value))
    case _ => Failure("Could not get bindings: " + map)
  }
}