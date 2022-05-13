package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object RetrieveType {
  def fromNewMapObject(nObject: NewMapObject, env: Environment): NewMapType = nObject match {
    case TaggedObject(_, nType) => nType
    case VersionedObjectLink(key, status) => {
      val currentState = Evaluator.currentState(key.uuid, env).toOption.get
      fromNewMapObject(currentState, env)
    }
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
    case BuildMapT(inputType, outputType, _) => {
      isTermClosedLiteral(inputType, knownVariables) &&
        isTermClosedLiteral(outputType, knownVariables)
    }
    case BuildGenericMapT(typeTransform, config) => isTermClosedLiteral(typeTransform, knownVariables)
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
    mapValues: Vector[(NewMapPattern, NewMapExpression)],
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
}