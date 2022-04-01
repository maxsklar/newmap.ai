package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object RetrieveType {
  // Every object has many types, but it's "official type" is in either how it's tagged or how it's defined
  def apply(nObject: NewMapObject, env: Environment): NewMapObject = nObject match {
    case Index(_) => CountT
    case IndexValue(_, indexT) => indexT 
    case CountT | SequenceT(_) | TypeT | AnyT | IdentifierT | StructT(_) | CaseT(_) | MapT(_, _, _, _) => TypeT
    case IdentifierInstance(s) => IdentifierT
    case IncrementFunc => MapT(CountT, CountT, RequireCompleteness, SimpleFunction)
    case SubtypeT(isMember) => this(retrieveInputTypeFromFunction(isMember, env), env)
    case MapInstance(values, mapT) => mapT
    case SequenceInstance(values, seqT) => seqT
    case ApplyFunction(func, input) => retrieveOutputTypeFromFunction(func, env)
    case AccessField(objectWithField, field) => {
      Evaluator.getCurrentConstantValue(objectWithField, env) match {
        case StructInstance(_, StructT(params)) => {
          // Field should be evaluated automatically (must be closed literal)
          // TODO - do we need to evaluate the params first - or will it be evaluated already?
          Evaluator.quickApplyFunctionAttempt(params, field, env) match {
            case Success(value) => value
            case Failure(s) => {
              throw new Exception(s"Field access retrieve type is poorly implemented!! $params -- $field -- $s")
            }
          }
        }
        case CaseT(values) => {
          Evaluator.quickApplyFunctionAttempt(values, field, env) match {
            case Success(value) => MapT(value, objectWithField, RequireCompleteness, SimpleFunction)
            case Failure(s) => {
              throw new Exception(s"Field access retrieve type is poorly implemented for case!! $values -- $field -- $s")
            }
          }
        }
        case _ => throw new Exception(s"This access of $objectWithField with field $field is not allowed")
      }
    }
    // TODO - unsafe get!!
    case ParamId(name) => {
      env.lookup(name) match {
        case None => throw new Exception(s"Attempted to retrieve type from an id that doesn't exist $name")
        case Some(EnvironmentValue(nObject, BoundStatus)) => this(nObject, env)
        case Some(EnvironmentValue(nObject, ParameterStatus)) => nObject
      }
    }
    case IsCommandFunc => MapT(TypeT, Index(2), CommandOutput, SimpleFunction)
    case IsSimpleFunction => MapT(AnyT, Index(2), CommandOutput, SimpleFunction)
    case IsVersionedFunc => MapT(AnyT, Index(2), CommandOutput, SimpleFunction)
    case IsConstantFunc => MapT(AnyT, Index(2), CommandOutput, SimpleFunction)
    case StructInstance(value, nType) => nType
    case CaseInstance(constructor, value, nType) => nType
    case VersionedObjectLink(_, _) => {
      this(Evaluator.getCurrentConstantValue(nObject, env), env)
    }
  }

  def retrieveInputTypeFromFunction(nFunction: NewMapObject, env: Environment): NewMapObject = {
    // TODO - eventually these mapinstances will have an automatic conversion to type (which is the key type)
    nFunction match {
      case VersionedObjectLink(_, _) => {
        retrieveInputTypeFromFunction(Evaluator.getCurrentConstantValue(nFunction, env), env)
      }
      case MapInstance(values, MapT(inputType, _, SubtypeInput, features)) => {
        SubtypeT(
          MapInstance(
            values.map(x => x._1 -> Index(1)),
            MapT(inputType, Index(2), CommandOutput, features)
          )
        )
      }
      case SequenceInstance(values, seqT) => {
        // TODO: Sequences shouldn't merely be based on indecies, each sequence should have it's own input tag
        Index(values.length)
      }
      case _ => {
        val nType = RetrieveType(nFunction, env)
        Evaluator(nType, env, false) match {
          case Success(MapT(inputType, _, _, _)) => inputType
          case other => throw new Exception(s"Couldn't retrieve input type from $nFunction -- $other")
        }
      }
    }
  }

  def retrieveFeatureSetFromFunction(nFunction: NewMapObject, env: Environment): MapFeatureSet = {
    // TODO - eventually these mapinstances will have an automatic conversion to type (which is the key type)
    nFunction match {
      case VersionedObjectLink(_, _) => {
        retrieveFeatureSetFromFunction(Evaluator.getCurrentConstantValue(nFunction, env), env)
      }
      case SequenceInstance(values, seqT) => BasicMap
      case _ => {
        val nType = RetrieveType(nFunction, env)
        Evaluator(nType, env, false) match {
          case Success(MapT(_, _, _, featureSet)) => featureSet
          case other => throw new Exception(s"Couldn't retrieve input type from $nFunction -- $other")
        }
      }
    }
  }

  def retrieveOutputTypeFromFunction(nFunction: NewMapObject, env: Environment): NewMapObject = {
    val nType = RetrieveType(nFunction, env)
    Evaluator(nType, env, false) match {
      case Success(MapT(_, outputType, _, _)) => outputType
      case Success(SequenceT(nType)) => nType
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
    case IdentifierInstance(_) | Index(_) | IndexValue(_, _) | IdentifierT | CountT | TypeT | AnyT | IncrementFunc | IsCommandFunc | IsVersionedFunc | IsConstantFunc | IsSimpleFunction => true
    case MapInstance(values, mapT) => {
      isMapValuesClosed(values, knownVariables)
    }
    case SequenceInstance(values, seqT) => {
      values.forall(value => isTermClosedLiteral(value, knownVariables))
    }
    case ParamId(name) => knownVariables.contains(name)
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
      )
    }
    case CaseInstance(constructor, input, caseType) => {
      isTermClosedLiteral(constructor, knownVariables) &&
        isTermClosedLiteral(input, knownVariables)
    }
    case MapT(inputType, outputType, _, _) => {
      isTermClosedLiteral(inputType, knownVariables) &&
        isTermClosedLiteral(outputType, knownVariables)
    }
    case SequenceT(nType) => {
      isTermClosedLiteral(nType, knownVariables)
    }
    case StructT(params) => isTermClosedLiteral(params, knownVariables)
    case CaseT(cases) => isTermClosedLiteral(cases, knownVariables)
    case SubtypeT(isMember) => isTermClosedLiteral(isMember, knownVariables)
    case VersionedObjectLink(_, _) => {
      // These are always closed literals! (I think)
      true
    }
  }

  def isMapValuesClosed(
    mapValues: Vector[(NewMapPattern, NewMapObject)],
    knownVariables: Vector[String]
  ): Boolean = {
    mapValues match {
      case (pattern, expression) +: restOfMapValues => {
        isPatternClosedLiteral(pattern, knownVariables) match {
          case Success(newKnownVariables) => {
            isTermClosedLiteral(expression, knownVariables ++ newKnownVariables) && isMapValuesClosed(restOfMapValues, knownVariables)
          }
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
            restOfKnownVariables <- isPatternClosedLiteral(StructPattern(restOfParams), newKnownVariables)
          } yield {
            newKnownVariables ++ restOfKnownVariables
          }
        }
        case _ => Success(Vector.empty)
      }
    }
    case CasePattern(constructor, input) => {
      isPatternClosedLiteral(input, knownVariables)
    }
  }

  // Ensures that the term is a constant
  def isTermConstant(nObject: NewMapObject): Boolean = {
    nObject match {
      case IdentifierInstance(_) | Index(_) | IndexValue(_, _) | IdentifierT | CountT | TypeT | AnyT | IncrementFunc | IsCommandFunc | IsVersionedFunc | IsConstantFunc | IsSimpleFunction => true
      case MapInstance(values, mapT) => isMapConstant(values)
      case SequenceInstance(values, seqT) => values.forall(value => isTermConstant(value))
      case ParamId(name) => true
      case ApplyFunction(func, input) => isTermConstant(func) && isTermConstant(input)
      case AccessField(struct, input) => isTermConstant(struct) && isTermConstant(input)
      case StructInstance(value, structType) => value.forall(x => isTermConstant(x._2))
      case CaseInstance(constructor, input, caseType) => isTermConstant(constructor) && isTermConstant(input)
      case MapT(inputType, outputType, _, _) => isTermConstant(inputType) && isTermConstant(outputType)
      case SequenceT(nType) => isTermConstant(nType)
      case StructT(params) => isTermConstant(params)
      case CaseT(cases) => isTermConstant(cases)
      case SubtypeT(isMember) => isTermConstant(isMember)
      case VersionedObjectLink(_, status) => (status == KeepThisVersion)
    }
  }

  def isMapConstant(
    mapValues: Vector[(NewMapPattern, NewMapObject)]
  ): Boolean = {
    mapValues match {
      case (pattern, expression) +: restOfMapValues => {
        isPatternConstant(pattern) && isTermConstant(expression) && isMapConstant(restOfMapValues)
      }
      case _ => true
    }
  }

  def isPatternConstant(nPattern: NewMapPattern): Boolean = nPattern match {
    case ObjectPattern(o) => {
      isTermConstant(o)
    }
    case TypePattern(name, nType) => {
      isTermConstant(nType)
    }
    case StructPattern(params) => {
      params match {
        case param +: restOfParams => {
          isPatternConstant(param) && isPatternConstant(StructPattern(restOfParams))
        }
        case _ => true
      }
    }
    case CasePattern(constructor, input) => {
      isPatternConstant(input)
    }
  }
}