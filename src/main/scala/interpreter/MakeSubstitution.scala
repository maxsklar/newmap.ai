package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Subsitute the given parameters for their given values in the expression
object MakeSubstitution {
  def apply(
    expression: NewMapObject,
    parameters: Map[String, NewMapObject],
    env: Environment
  ): NewMapObject = {
    expression match {
      case CountT | TypeT | AnyT | IdentifierT | Index(_) | IndexValue(_, _) | IdentifierInstance(_) | VersionedObjectLink(_, _) => expression
      case MapT(inputType, outputType, completeness, featureSet) => {
        MapT(
          this(inputType, parameters, env),
          this(outputType, parameters, env),
          completeness,
          featureSet
        )
      }
      case SequenceT(nType) => {
        SequenceT(this(nType, parameters, env))
      }
      case StructT(values) => {
        StructT(this(values, parameters, env))
      }
      case CaseT(cases) => {
        CaseT(this(cases, parameters, env))
      }
      case SubtypeT(isMember) => {
        SubtypeT(this(isMember, parameters, env))
      }
      case ApplyFunction(func, input) => {
        ApplyFunction(
          this(func, parameters, env),
          this(input, parameters, env)
        )
      }
      case AccessField(struct, field) => {
        AccessField(
          this(struct, parameters, env),
          this(field, parameters, env)
        )
      }
      case ParamId(name) => {
        parameters.get(name) match {
          case Some(obj) => obj
          case None => expression
        }
      }
      case StructInstance(value, structT) => {
        StructInstance(
          value.map(x => (x._1 -> this(x._2, parameters, env))),
          structT
        )
      }
      case CaseInstance(constructor, value, caseT) => {
        CaseInstance(constructor, this(value, parameters, env), caseT)
      }
      case IsCommandFunc | IsSimpleFunction | IsVersionedFunc | IsConstantFunc | IncrementFunc => expression
      case MapInstance(values, mapT) => {
        val newValues = for {
          (k, v) <- values
          substK = substPattern(k, parameters, env)

          internalParams = Evaluator.newParametersFromPattern(substK)
          // We cannot replace params that have the same name

          remainingParameters = parameters -- internalParams.map(_._1)
        } yield (substK -> this(v, remainingParameters, env))

        MapInstance(
          newValues,
          mapT
        )
      }
      case SequenceInstance(values, seqT) => {
        val newValues = for {
          value <- values
        } yield MakeSubstitution(value, parameters, env)

        SequenceInstance(newValues, seqT)
      }
    }
  }

  def substPattern(
    pattern: NewMapPattern,
    parameters: Map[String, NewMapObject],
    env: Environment
  ): NewMapPattern = pattern match {
    case ObjectPattern(oPattern) => ObjectPattern(this(oPattern, parameters, env))
    case TypePattern(name, nType) => TypePattern(name, this(nType, parameters, env))
    case StructPattern(params) => {
      StructPattern(params.map(param => substPattern(param, parameters, env)))
    }
    case CasePattern(constructor, input) => {
      CasePattern(constructor, substPattern(input, parameters, env))
    }
  }
}