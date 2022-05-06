package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object RetrieveType {
  // Every object has many types, but it's "official type" is in either how it's tagged or how it's defined
  /*def apply(nExpression: NewMapExpression, env: Environment): NewMapObject = nExpression match {
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
  }*/

  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)

  def fromNewMapObject(nObject: NewMapObject, env: Environment): NewMapType = nObject match {
    //case CountT | ExpandingSubsetT(_, _) | DataTypeT(_) | TypeT | AnyT | IdentifierT | StructT(_, _) | CaseT(_, _) | MapT(_, _, _) | OrBooleanT => TypeT
    //case SubtypeT(isMember) => this(retrieveInputTypeFromFunction(isMember, env), env)
    //case SubtypeT(isMember) => TypeT // Is this right?
    case TaggedObject(_, nType) => nType
    case VersionedObjectLink(key, status) => {
      val currentState = Evaluator.currentState(key.uuid, env).toOption.get
      fromNewMapObject(currentState, env)
    }
  }

  def retrieveInputTypeFromFunctionObj(nFunction: NewMapObject, env: Environment): NewMapType = {
    inputTypeFromFunctionType(RetrieveType.fromNewMapObject(nFunction, env), env)
  }

  def inputTypeFromFunctionType(nFunctionType: NewMapType, env: Environment): NewMapType = {
    Evaluator.stripCustomTag(nFunctionType) match {
      case MapT(inputType, _, _) => inputType
      case StructT(params, parentFieldType, featureSet, _) => {
        SubtypeT(UMap(params), parentFieldType, featureSet)
        
        //TaggedObject(params, ExpandingSubsetT(parentFieldType, true))
        //inputTypeFromFunctionType(RetrieveType.fromNewMapObject(params, env), env)
      }
      case TypeClassT(typeTransform, typesInTypeClass) => {
        SubtypeT(
          UMap(typesInTypeClass.map(x => (x -> ObjectExpression(UIndex(1))))),
          TypeT,
          SimpleFunction
        )
      }
      case other => throw new Exception(s"Couldn't retrieve input type from $nFunctionType -- $other")
    }
  }

  def retrieveFeatureSetFromFunctionType(nType: NewMapType, env: Environment): Outcome[MapFeatureSet, String] = {
    nType match {
      case MapT(_, _, config) => Success(config.featureSet)
      case StructT(_, _, featureSet, _) => Success(featureSet)
      case TypeClassT(_, _) => Success(SimpleFunction)
      case CustomT(_, t) => retrieveFeatureSetFromFunctionType(t, env)
      case _ => Failure(s"Cannot retrieve meaningful feature set from object with type $nType")
    }
  }

  def outputTypeFromFunctionType(nType: NewMapType, env: Environment): NewMapType = {
    Evaluator.stripCustomTag(nType) match {
      case MapT(_, outputType, _) => outputType
      /*case VersionedObjectLink(key, status) => {
        val currentState = Evaluator.currentState(key.uuid, env).toOption.get
        outputTypeFromFunctionType(currentState, env)
      }*/
      //case ExpandingSubsetT(_, _) => OrBooleanT
      case _ => throw new Exception(s"Couldn't retrieve output type from $nType")
    }
  }

  /*def retrieveOutputTypeFromStruct(structValue: NewMapExpression, field: NewMapExpression, env: Environment): Outcome[NewMapObject, String] = {
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
  }*/

  def getParentType(nType: NewMapType, env: Environment): NewMapType = {
    nType match {
      /*case SubtypeT(isMember) => {
        getParentType(retrieveInputTypeFromFunctionObj(isMember, env), env)
      }*/
      /*case VersionedObjectLink(key, status) => {
        val currentState = Evaluator.currentState(key.uuid, env).toOption.get
        getParentType(currentState, env)
      }*/
      //case TaggedObject(_, ExpandingSubsetT(parentType, _)) => getParentType(parentType, env)
      case SubtypeT(_, parentType, _) => getParentType(parentType, env)
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
    case BuildCase(_, input) => isTermClosedLiteral(input, knownVariables)
    case BuildMapT(inputType, outputType, _) => {
      isTermClosedLiteral(inputType, knownVariables) &&
        isTermClosedLiteral(outputType, knownVariables)
    }
    case BuildTableT(keyType, requiredValues) => {
      isTermClosedLiteral(keyType, knownVariables) && isTermClosedLiteral(requiredValues, knownVariables)
    }
    /*case BuildExpandingSubsetT(parentType, _) => {
      isTermClosedLiteral(parentType)
    }*/
    case BuildSubtypeT(isMember, _, _) => isTermClosedLiteral(isMember, knownVariables)
    case BuildCaseT(cases, _, _) => isTermClosedLiteral(cases, knownVariables)
    case BuildStructT(params, _, _) => isTermClosedLiteral(params, knownVariables)
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