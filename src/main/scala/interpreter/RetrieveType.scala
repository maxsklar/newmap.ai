package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object RetrieveType {
  // Every object has many types, but it's "official type" is in either how it's tagged or how it's defined
  def apply(nExpression: NewMapExpression, env: Environment): NewMapObject = nExpression match {
    case ObjectExpression(nObject) => fromNewMapObject(nObject, env)
    case ApplyFunction(func, input) => {
      // TODO - merge these options soon
      retrieveOutputTypeFromStruct(func, input, env) match {
        case Success(t) => t
        case Failure(_) => {
          val typeOfFunction = this(func, env)
          retrieveOutputTypeFromFunctionType(typeOfFunction, env) 
        }
      }
    }
    case ParamId(name) => {
      env.lookup(name) match {
        case None => throw new Exception(s"Attempted to retrieve type from an id that doesn't exist $name")
        case Some(EnvironmentValue(nObject, BoundStatus)) => fromNewMapObject(nObject, env)
        case Some(EnvironmentValue(nObject, ParameterStatus)) => nObject
      }
    }
    case BuildCase(_, _, caseType) => caseType
    case BuildMapT(_, _, _) | BuildTableT(_, _) | BuildExpandingSubsetT(_, _) | BuildSubtypeT(_) | BuildCaseT(_) | BuildStructT(_) => TypeT
    case BuildMapInstance(values, mapT) => mapT // TODO - what if this is a submap???
  }

  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)

  def fromNewMapObject(nObject: NewMapObject, env: Environment): NewMapObject = nObject match {
    case CountT | ExpandingSubsetT(_, _) | TypeT | AnyT | IdentifierT | StructT(_) | CaseT(_) | MapT(_, _, _) | OrBooleanT => TypeT
    //case SubtypeT(isMember) => this(retrieveInputTypeFromFunction(isMember, env), env)
    case SubtypeT(isMember) => TypeT // Is this right?
    case TaggedObject(_, nType) => nType
    case VersionedObjectLink(key, status) => {
      val currentState = Evaluator.currentState(key.uuid, env).toOption.get
      fromNewMapObject(currentState, env)
    }
  }

  def retrieveInputTypeFromFunctionObj(nFunction: NewMapObject, env: Environment): NewMapObject = {
    inputTypeFromFunctionType(RetrieveType.fromNewMapObject(nFunction, env), env)
  }

  def inputTypeFromFunctionType(nFunctionType: NewMapObject, env: Environment): NewMapObject = {
    Evaluator.stripVersioning(nFunctionType, env) match {
      case MapT(inputType, _, _) => inputType
      case StructT(params) => inputTypeFromFunctionType(RetrieveType.fromNewMapObject(params, env), env)
      case other => throw new Exception(s"Couldn't retrieve input type from $nFunctionType -- $other")
    }
  }

  def retrieveFeatureSetFromFunction(nFunction: NewMapExpression, env: Environment): MapFeatureSet = {
    nFunction match {
      // TODO - again this CaseT exception is really looking ugly!!!
      case ObjectExpression(VersionedObjectLink(key, status)) => {
        val currentState = Evaluator.currentState(key.uuid, env).toOption.get
        retrieveFeatureSetFromFunction(ObjectExpression(currentState), env)
      }
      case _ => {
        val typeOfFunction = this(nFunction, env)
        val typeOfFunctionC = Evaluator.stripVersioning(typeOfFunction, env)
        typeOfFunctionC match {
          case MapT(_, _, config) => config.featureSet
          case StructT(params) => retrieveFeatureSetFromFunction(ObjectExpression(params), env)
          case other => throw new Exception(s"Couldn't retrieve feature set from $nFunction $typeOfFunctionC -- $other")
        }
      }
    }
  }

  def retrieveOutputTypeFromFunctionType(nType: NewMapObject, env: Environment): NewMapObject = {
    nType match {
      case MapT(_, outputType, _) => outputType
      case VersionedObjectLink(key, status) => {
        val currentState = Evaluator.currentState(key.uuid, env).toOption.get
        retrieveOutputTypeFromFunctionType(currentState, env)
      }
      case ExpandingSubsetT(_, _) => OrBooleanT
      case _ => throw new Exception(s"Couldn't retrieve output type from $nType")
    }
  }

  def retrieveOutputTypeFromStruct(structValue: NewMapExpression, field: NewMapExpression, env: Environment): Outcome[NewMapObject, String] = {
    structValue match {
      case ObjectExpression(nObject) => Evaluator.stripVersioning(nObject, env) match {
        case TaggedObject(_, StructT(params)) => {
          // Field should be evaluated automatically (must be closed literal)
          // TODO - do we need to evaluate the params first - or will it be evaluated already?
          for {
            eField <- Evaluator(field, env)
            result <- Evaluator.applyFunctionAttempt(params, eField, env)
          } yield result
        }
        case _ => Failure(s"This access of object $structValue with field $field is not allowed")
      }
      // TODO - what if it's a different expression, but comes out to a struct, case, etc
      case _ => Failure(s"This access of $structValue with field $field is not allowed")
    }
  }

  def getParentType(nType: NewMapObject, env: Environment): NewMapObject = {
    nType match {
      case SubtypeT(isMember) => {
        getParentType(retrieveInputTypeFromFunction(ObjectExpression(isMember), env), env)
      }
      case VersionedObjectLink(key, status) => {
        val currentState = Evaluator.currentState(key.uuid, env).toOption.get
        getParentType(currentState, env)
      }
      case TaggedObject(_, ExpandingSubsetT(parentType, _)) => getParentType(parentType, env)
      case t => t
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
    case BuildCase(_, input, _) => isTermClosedLiteral(input, knownVariables)
    case BuildMapT(inputType, outputType, _) => {
      isTermClosedLiteral(inputType, knownVariables) &&
        isTermClosedLiteral(outputType, knownVariables)
    }
    case BuildTableT(keyType, requiredValues) => {
      isTermClosedLiteral(keyType, knownVariables) && isTermClosedLiteral(requiredValues, knownVariables)
    }
    case BuildExpandingSubsetT(parentType, _) => {
      isTermClosedLiteral(parentType)
    }
    case BuildSubtypeT(isMember) => isTermClosedLiteral(isMember, knownVariables)
    case BuildCaseT(cases) => isTermClosedLiteral(cases, knownVariables)
    case BuildStructT(params) => isTermClosedLiteral(params, knownVariables)
    case BuildMapInstance(values, mapT) => {
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

  // Ensures that the term is a constant
  def isTermConstant(nObject: NewMapObject): Boolean = {
    nObject match {
      case IdentifierT | CountT | TypeT | AnyT | OrBooleanT => true
      case MapT(inputType, outputType, _) => isTermConstant(inputType) && isTermConstant(outputType)
      case ExpandingSubsetT(parentType, _) => isTermConstant(parentType)
      case StructT(params) => isTermConstant(params)
      case CaseT(cases) => isTermConstant(cases)
      case SubtypeT(isMember) => isTermConstant(isMember)
      case TaggedObject(_, nType) => true
      case VersionedObjectLink(_, status) => (status == KeepThisVersion)
    }
  }
}