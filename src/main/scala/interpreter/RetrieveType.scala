package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object RetrieveType {
  // Every object has many types, but it's "official type" is in either how it's tagged or how it's defined
  def apply(nObject: NewMapObject, env: Environment): NewMapObject = nObject match {
    case Index(_) => CountT
    case CountT | TypeT | AnyT | IdentifierT | StructT(_) | CaseT(_) | MapT(_, _, _, _) => TypeT
    case IdentifierInstance(s) => IdentifierT
    case RangeFunc(i) => MapT(CountT, NewMapO.rangeT(2), CommandOutput, BasicMap)
    case IncrementFunc => MapT(CountT, CountT, RequireCompleteness, SimpleFunction)
    case SubtypeT(isMember) => this(retrieveInputTypeFromFunction(isMember, env), env)
    case MapInstance(values, mapT) => mapT
    case ApplyFunction(func, input) => retrieveOutputTypeFromFunction(func, env)
    case AccessField(StructInstance(_, StructT(params)), field) => {
      // Field should be evaluated automatically (not allowed to have a ParameterObj in there - must be closed literal)
      // TODO - do we need to evaluate the params first - or will it be evaluated already?
      Evaluator.quickApplyFunctionAttempt(params, field, env) match {
        case Success(value) => value
        case Failure(s) => {
          throw new Exception(s"Field access retrieve type is poorly implemented!! $params -- $field -- $s")
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
    // TODO - unsafe get!!
    case ParamId(name) => this(env.lookup(name).get, env)
    case ParameterObj(_, nType) => nType
    case IsCommandFunc => MapT(TypeT, NewMapO.rangeT(2), CommandOutput, SimpleFunction)
    case IsSimpleFunction => MapT(AnyT, NewMapO.rangeT(2), CommandOutput, SimpleFunction)
    case StructInstance(value, nType) => nType
    case CaseInstance(constructor, value, nType) => nType
  }

  def retrieveInputTypeFromFunction(nFunction: NewMapObject, env: Environment): NewMapObject = {
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
     case _ => RetrieveType(nFunction, env) match {
     // Once lambdaInstance is eliminated, these lines get easier to write!
        case MapT(inputType, _, _, _) => inputType
        case _ => throw new Exception(s"Couldn't retrieve input type from $nFunction")
      }
    }
  }

  def retrieveOutputTypeFromFunction(nFunction: NewMapObject, env: Environment): NewMapObject = {
    RetrieveType(nFunction, env) match {
      case MapT(_, outputType, _, _) => outputType
      case _ => throw new Exception(s"Couldn't retrieve output type from $nFunction")
    }
  }

  def getParentType(nType: NewMapObject, env: Environment): NewMapObject = {
    nType match {
      case SubtypeT(isMember) => {
        getParentType(retrieveInputTypeFromFunction(isMember, env), env)
      }
      case t => t
    }
  }

  // Ensures that there are no free variables in this term
  def isTermClosedLiteral(
    nObject: NewMapObject,
    knownVariables: Vector[String] = Vector.empty,
  ): Boolean = nObject match {
    case IdentifierInstance(_) | Index(_) | IdentifierT | CountT | TypeT | AnyT | RangeFunc(_) | IncrementFunc | IsCommandFunc | IsSimpleFunction => true
    case MapInstance(values, mapT) => {
      isMapValuesClosed(values, knownVariables) && isTermClosedLiteral(mapT, knownVariables)
    }
    case ParamId(name) => knownVariables.contains(name)
    case ParameterObj(_, _) => false
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

  def isMapValuesClosed(
    mapValues: Vector[(NewMapPattern, NewMapObject)],
    knownVariables: Vector[String]
  ): Boolean = {
    mapValues match {
      case (pattern, expression) +: restOfMapValues => {
        isPatternClosedLiteral(pattern, knownVariables) match {
          case Success(newKnownVariables) => isTermClosedLiteral(expression, knownVariables ++ newKnownVariables)
          case Failure(_) => false
        }
      }
      case _ => true
    }
  }

  def isPatternClosedLiteral(
    nPattern: NewMapPattern,
    knownVariables: Vector[String] = Vector.empty,
  ): Outcome[Vector[String], String] = nPattern match {
    case ObjectPattern(o) => {
      if (isTermClosedLiteral(o, knownVariables)) Success(Vector.empty)
      else Failure(s"term not closed: $o")
    }
    case TypePattern(name, nType) => {
      if (isTermClosedLiteral(nType)) Success(Vector(name))
      else Failure(s"type not closed $nType")
    }
    case StructPattern(params) => {
      params match {
        case param +: restOfParams => {
          for {
            newKnownVariables <- isPatternClosedLiteral(param)
            restOfKnownVariables <- isPatternClosedLiteral(StructPattern(params), newKnownVariables)
          } yield {
            newKnownVariables ++ restOfKnownVariables
          }
        }
        case _ => Success(Vector.empty)
      }
    }
    case MapTPattern(input, output, featureSet) => {
      // TODO: Implement
      Failure("Unimplemented")
    }
    case MapPattern(mapTPattern) => {
      isPatternClosedLiteral(mapTPattern, knownVariables)
    }
  }
}