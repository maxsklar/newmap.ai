package ai.newmap.interpreter

import ai.newmap.model._

object RetrieveType {
  // Every object has many types, but it's "official type" is in either how it's tagged or how it's defined
  def apply(nObject: NewMapObject): NewMapSubtype = nObject match {
    case Index(_) => CountT
    case CountT | TypeT | AnyT | IdentifierT | StructT(_) | CaseT(_) | MapT(_, _, _, _) => TypeT
    case IdentifierInstance(s) => IdentifierT
    case RangeFunc(i) => MapT(CountT, NewMapO.rangeT(2), CommandOutput, BasicMap)
    case SubtypeT(isMember) => this(retrieveInputTypeFromFunction(isMember))
    case MapInstance(values, mapT) => mapT
    case LambdaInstance(params, expression) => {
      val inputType = params match {
        case StructParams(params) => {
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

          StructT(structParams)
        }
        case IdentifierParam(_, nType) => nType
      }

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
      Evaluator.quickApplyFunctionAttempt(values, field, Environment.Base).toOption match {
        case Some(value: NewMapSubtype) => MapT(value, caseT, RequireCompleteness, SimpleFunction)
        case Some(value) => {
          throw new Exception(s"Field access retrieve type is poorly implemented for case!! $values -- $value-- $field")
        }
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
    case SubstitutableT(s, nType) => nType
  }

  def retrieveInputTypeFromFunction(nFunction: NewMapObject): NewMapSubtype = nFunction match {
    case MapInstance(values, MapT(inputType, outputType, completeness, _)) => {
      inputType
    }
    case LambdaInstance(params, expression) => {
      params match {
        case StructParams(params) => {
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

          StructT(structParams)
        }
        case IdentifierParam(_, nType) => nType
      }
    }
    case ParameterObj(s, MapT(inputType, _, _, _)) => inputType
    case IsCommandFunc => TypeT
    case RangeFunc(i) => CountT
    case AccessField(CaseT(cases), field) => {
      // TODO - pass in environment? Have an environment-less version?
      Evaluator.quickApplyFunctionAttempt(cases, field, Environment.Base).toOption match {
        case Some(result) => Evaluator.convertObjectToType(result, Environment.Base).toOption match {
          case Some(resultT) => resultT
          case None => throw new Exception("This will be going away soon")
        }
        case None => {
          throw new Exception(s"unable to find case $field in $cases")
        }
      }
    }
    case _ => {
      throw new Exception(s"Couldn't retrieve input type from $nFunction")
    }
  }

  def retrieveOutputTypeFromFunction(nFunction: NewMapObject): NewMapSubtype = nFunction match {
    case MapInstance(values, MapT(inputType, outputType, completeness, _)) => {
      outputType
    }
    case LambdaInstance(params, expression) => {
      // TODO: Work has to be done here to not break generics
      this(expression)
    }
    case ParameterObj(s, MapT(_, outputType, _, _)) => outputType
    case IsCommandFunc => NewMapO.rangeT(2)
    case RangeFunc(i) => NewMapO.rangeT(2)
    case AccessField(caseT@CaseT(cases), field) => {
      caseT
    }
    case _ => {
      throw new Exception(s"Couldn't retrieve output type from $nFunction")
    }
  }

  def getParentType(nType: NewMapSubtype): NewMapType = {
    nType match {
      case SubtypeT(isMember) => {
        getParentType(retrieveInputTypeFromFunction(isMember))
      }
      case t: NewMapType => t
    }
  }

  // Ensures that there are no free variables in this term
  def isTermClosedLiteral(
    nObject: NewMapObject,
    knownVariables: Vector[String] = Vector.empty,
  ): Boolean = nObject match {
    case IdentifierInstance(_) | Index(_) | IdentifierT | CountT | TypeT | AnyT | RangeFunc(_) | IsCommandFunc => true
    case MapInstance(values, mapT) => {
      values.forall(v =>
        isTermClosedLiteral(v._1, knownVariables) &&
        isTermClosedLiteral(v._2, knownVariables)
      ) && isTermClosedLiteral(mapT, knownVariables)
    }
    case ParameterObj(name, _) => knownVariables.contains(name)
    case LambdaInstance(StructParams(params), expression) => {
      // TODO - change this when we start using de bruin (or other)codes for params
      val newParams = params.flatMap(x => x._1 match {
        case IdentifierInstance(s) => Some(s)
        case _ => None
      })
      isTermClosedLiteral(expression, knownVariables ++ newParams)
    }
    case LambdaInstance(IdentifierParam(name, _), expression) => {
      // TODO - change this when we start using de bruin (or other) codes for params
      isTermClosedLiteral(expression, knownVariables :+ name)
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
    case SubstitutableT(s, _) => knownVariables.contains(s)
    case SubtypeT(isMember) => isTermClosedLiteral(isMember, knownVariables)
  }
}