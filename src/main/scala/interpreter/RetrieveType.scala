package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object RetrieveType {
  def fromNewMapObject(nObject: NewMapObject, env: Environment): NewMapType = nObject match {
    case TaggedObject(_, nType) => nType
    case VersionedObjectLink(key) => {
      val currentState = Evaluator.currentState(key.uuid, env).toOption.get
      fromNewMapObject(currentState, env)
    }
  }

  def isTermPatternFree(untaggedObject: UntaggedObject): Boolean = untaggedObject match {
    case UWildcardPattern(_) => false
    case UCase(_, input) => isTermPatternFree(input)
    case UStruct(patterns) => patterns.forall(isTermPatternFree(_))
    case _ => true
  }

  // Ensures that there are no free variables in this term
  // TODO: Return a NewMapObject if this is the case?
  // TODO: This can be handled by a specialized type-checker on NewMapExpression (ripe for removing this code)
  // - But it's not that simple yet
  def isTermClosedLiteral(
    nExpression: NewMapExpression,
    knownVariables: Vector[String] = Vector.empty,
  ): Boolean = nExpression match {
    case ObjectExpression(_) => true
    case ParamId(name) => knownVariables.contains(name)
    case ApplyFunction(func, input) => {
      isTermClosedLiteral(func, knownVariables) &&
        isTermClosedLiteral(input, knownVariables)
    }
    case BuildCase(_, input) => isTermClosedLiteral(input, knownVariables)
    case BuildSimpleMapT(inputExp, outputExp, env) => {
      isTermClosedLiteral(inputExp, knownVariables) && isTermClosedLiteral(outputExp, knownVariables)
    }
    case BuildMapT(typeTransform, config) => isTermClosedLiteral(typeTransform, knownVariables)
    case BuildTableT(keyType, requiredValues) => {
      isTermClosedLiteral(keyType, knownVariables) && isTermClosedLiteral(requiredValues, knownVariables)
    }
    case BuildSubtypeT(isMember, _, _) => isTermClosedLiteral(isMember, knownVariables)
    case BuildCaseT(cases, _, _) => isTermClosedLiteral(cases, knownVariables)
    case BuildStructT(params, _, _, _) => isTermClosedLiteral(params, knownVariables)
    case BuildNewTypeClassT(typeTransform) => isTermClosedLiteral(typeTransform)
    case BuildMapInstance(values) => {
      isMapValuesClosed(values, knownVariables)
    }
  }

  def isMapValuesClosed(
    mapValues: Vector[(UntaggedObject, NewMapExpression)],
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
            valueTypeExpression <- Evaluator.attemptPatternMatchInOrder(cases, constructor, env)
            valueType <- Evaluator(valueTypeExpression, env)
            valueT <- env.typeSystem.convertToNewMapType(valueType)
            result <- fetchParamsFromPattern(valueT, resultPattern, env)
          } yield result
        }
        case (StructT(params, _, _, BasicMap), UStruct(values)) => {
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
}