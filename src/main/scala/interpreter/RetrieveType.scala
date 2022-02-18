package ai.newmap.interpreter

import ai.newmap.model._

object RetrieveType {
  // Every object has many types, but it's "official type" is in either how it's tagged or how it's defined
  // TODO - Accept environment
  def apply(nObject: NewMapObject): NewMapObject = nObject match {
    case Index(_) => CountT
    case CountT | TypeT | AnyT | IdentifierT | StructT(_) | CaseT(_) | MapT(_, _, _, _) => TypeT
    case IdentifierInstance(s) => IdentifierT
    case IdentifierPattern(_, nType) => nType
    case RangeFunc(i) => MapT(CountT, NewMapO.rangeT(2), CommandOutput, BasicMap)
    case IncrementFunc => MapT(CountT, CountT, RequireCompleteness, SimpleFunction)
    case SubtypeT(isMember) => this(retrieveInputTypeFromFunction(isMember))
    case MapInstance(values, mapT) => mapT
    case LambdaInstance(params, expression) => {
      val fieldType = {
        SubtypeT(
          MapInstance(
            params.map(x => x._1 -> Index(1)),
            MapT(IdentifierT, NewMapO.rangeT(2), CommandOutput, BasicMap)
          )
        )
      }

      val structParams = MapInstance(
        params.map(x => x._1 -> x._2),
        MapT(fieldType, TypeT, RequireCompleteness, BasicMap)
      )


      val inputType = StructT(structParams)

      // TODO - this should take a new environment
      val outputType = this(expression)

      MapT(inputType, outputType, RequireCompleteness, BasicMap)
    }
    case ApplyFunction(func, input) => retrieveOutputTypeFromFunction(func)
    case AccessField(StructInstance(values, _), field) => {
      // TODO: Improve!
      values.find(v => v._1 == field).map(_._2) match {
        case Some(value) => this(value)
        case None => {
          // This should happen if the field has been confirmed closed and fully evaluated
          throw new Exception(s"Field access retrieve type is poorly implemented!! $values -- $field")
        }
      }
    }
    case AccessField(caseT@CaseT(values), field) => {
      // TODO - I really don't like using Environment.Base here!
      // - perhaps there should be an environment-less apply function attempt since caseT should already be literal
      // - Hold off on fixing this, because in the future, the apply function might not require this!
      Evaluator.quickApplyFunctionAttempt(values, field, Environment.Base).toOption match {
        case Some(value) => MapT(value, caseT, RequireCompleteness, SimpleFunction)
        case _ => {
          throw new Exception(s"Field access retrieve type is poorly implemented for case!! $values -- $field")
        }
      }
    }
    case AccessField(struct, field) => {
      throw new Exception(s"This access of $struct with field $field is not allowed")
    }
    case ParameterObj(_, nType) => nType
    case IsCommandFunc => MapT(TypeT, NewMapO.rangeT(2), CommandOutput, SimpleFunction)
    case StructInstance(value, nType) => nType
    case CaseInstance(constructor, value, nType) => nType
  }

  def retrieveInputTypeFromFunction(nFunction: NewMapObject): NewMapObject = {
    // TODO - eventually these mapinstances will have an automatic conversion to type (which is the key type)
    nFunction match {
      case MapInstance(values, MapT(inputType, _, SubtypeInput, features)) => {
        SubtypeT(
          MapInstance(
            values.map(x => x._1 -> Index(1)),
            MapT(inputType, NewMapO.rangeT(2), CommandOutput, features)
          )
        )
      }
     case _ => RetrieveType(nFunction) match {
     // Once lambdaInstance is eliminated, these lines get easier to write!
        case MapT(inputType, _, _, _) => inputType
        case _ => throw new Exception(s"Couldn't retrieve input type from $nFunction")
      }
    }
  }

  def retrieveOutputTypeFromFunction(nFunction: NewMapObject): NewMapObject = {
    RetrieveType(nFunction) match {
      case MapT(_, outputType, _, _) => outputType
      case _ => throw new Exception(s"Couldn't retrieve output type from $nFunction")
    }
  }

  def getParentType(nType: NewMapObject): NewMapObject = {
    nType match {
      case SubtypeT(isMember) => {
        getParentType(retrieveInputTypeFromFunction(isMember))
      }
      case t => t
    }
  }

  // Ensures that there are no free variables in this term
  def isTermClosedLiteral(
    nObject: NewMapObject,
    knownVariables: Vector[String] = Vector.empty,
  ): Boolean = nObject match {
    case IdentifierInstance(_) | Index(_) | IdentifierT | CountT | TypeT | AnyT | RangeFunc(_) | IncrementFunc | IsCommandFunc => true
    case MapInstance(values, mapT) => {
      values.forall(v =>
        isTermClosedLiteral(v._1, knownVariables) &&
        isTermClosedLiteral(v._2, knownVariables)
      ) && isTermClosedLiteral(mapT, knownVariables)
    }
    case ParameterObj(name, _) => knownVariables.contains(name)
    case IdentifierPattern(_, _) => false
    case LambdaInstance(params, expression) => {
      // TODO - change this when we start using de bruin (or other)codes for params
      val newParams = params.flatMap(x => x._1 match {
        case IdentifierInstance(s) => Some(s)
        case _ => None
      })
      isTermClosedLiteral(expression, knownVariables ++ newParams)
    }
    case ApplyFunction(func, input) => {
      isTermClosedLiteral(func, knownVariables) &&
        isTermClosedLiteral(input, knownVariables)
    }
    case AccessField(struct, input) => {
      // TODO: If this were the case, wouldn't this have already been evaluated?
      isTermClosedLiteral(struct, knownVariables) &&
        isTermClosedLiteral(input, knownVariables)
    }
    case StructInstance(value, structType) => {
      value.forall(x =>
        isTermClosedLiteral(x._2, knownVariables)
      ) && isTermClosedLiteral(structType, knownVariables)
    }
    case CaseInstance(constructor, input, caseType) => {
      isTermClosedLiteral(constructor, knownVariables) &&
        isTermClosedLiteral(input, knownVariables) &&
        isTermClosedLiteral(caseType, knownVariables)
    }
    case MapT(inputType, outputType, _, _) => {
      isTermClosedLiteral(inputType, knownVariables) &&
        isTermClosedLiteral(outputType, knownVariables)
    }
    case StructT(params) => isTermClosedLiteral(params, knownVariables)
    case CaseT(cases) => isTermClosedLiteral(cases, knownVariables)
    case SubtypeT(isMember) => isTermClosedLiteral(isMember, knownVariables)
  }
}