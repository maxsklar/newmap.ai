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
      case CountT | TypeT | AnyT | IdentifierT | ParameterObj(_, _) | Index(_) | IdentifierInstance(_) => expression
      case MapT(inputType, outputType, completeness, featureSet) => {
        MapT(
          this(inputType, parameters, env),
          this(outputType, parameters, env),
          completeness,
          featureSet
        )
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
      case StructInstance(value, StructT(p)) => {
        StructInstance(
          value.map(x => (x._1 -> this(x._2, parameters, env))),
          StructT(this(p, parameters, env))
        )
      }
      case CaseInstance(constructor, value, CaseT(c)) => {
        CaseInstance(constructor, this(value, parameters, env), CaseT(this(c, parameters, env)))
      }
      case IsCommandFunc | IsSimpleFunction | IsVersionedFunc | RangeFunc(_) | IncrementFunc => expression
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
          MapT(
            this(mapT.inputType, parameters, env),
            this(mapT.outputType, parameters, env),
            mapT.completeness,
            mapT.featureSet
          )
        )
      }
      case VersionedObject(currentState: NewMapObject, commandType: NewMapObject, versionNumber: Long) => {
        VersionedObject(
          this(currentState, parameters, env),
          this(commandType, parameters, env),
          versionNumber
        )
      }
      case BranchedVersionedObject(vo: NewMapObject, base: NewMapObject, changeLog: Vector[NewMapObject]) => {
        BranchedVersionedObject(
          this(vo, parameters, env),
          this(base, parameters, env),
          changeLog.map(c => this(c, parameters, env))
        )
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
    case MapTPattern(input, output, featureSet) => {
      MapTPattern(
        substPattern(input, parameters, env),
        substPattern(output, parameters, env),
        featureSet
      )
    }
    case MapPattern(MapTPattern(input, output, featureSet)) => {
      MapPattern(MapTPattern(
        substPattern(input, parameters, env),
        substPattern(output, parameters, env),
        featureSet
      ))
    }
  }
}