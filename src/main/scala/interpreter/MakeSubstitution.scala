package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Subsitute the given parameters for their given values in the expression
object MakeSubstitution {
  def apply(
    expression: NewMapExpression,
    parameters: Map[String, NewMapExpression],
    env: Environment
  ): NewMapExpression = {
    expression match {
      case ObjectExpression(nObject) => {
        // Temporary solution is to dig through to find the map expressions with the parameters
        // Permanent solution is to make a "build map construction + functions"
        val fixedObject = substObject(nObject, parameters, env)
        ObjectExpression(fixedObject)
      }
      case ApplyFunction(func, input) => {
        ApplyFunction(
          this(func, parameters, env),
          this(input, parameters, env)
        )
      }
      case ParamId(name) => {
        parameters.get(name) match {
          case Some(nExpression) => (nExpression)
          case None => expression
        }
      }
      case BuildCase(constructor, input) => {
        BuildCase(constructor, this(input, parameters, env))
      }
      case BuildMapT(inputType, outputType, config) => {
        BuildMapT(
          this(inputType, parameters, env),
          this(outputType, parameters, env),
          config
        )
      }
      case BuildGenericMapT(typeTransform, config) => {
        BuildGenericMapT(this(typeTransform, parameters, env), config)
      }
      case BuildTableT(keyType, requiredValues) => {
        BuildTableT(
          this(keyType, parameters, env),
          this(requiredValues, parameters, env)
        )
      }
      case BuildSubtypeT(isMember, parentType, featureSet) => {
        BuildSubtypeT(this(isMember, parameters, env), this(parentType, parameters, env), featureSet)
      }
      case BuildCaseT(cases, parentFieldType, featureSet) => BuildCaseT(this(cases, parameters, env), parentFieldType, featureSet)
      case BuildStructT(params, parentFieldType, completeness, featureSet) => BuildStructT(this(params, parameters, env), parentFieldType, completeness, featureSet)
      case BuildNewTypeClassT(typeTransform) => BuildNewTypeClassT(this(typeTransform, parameters, env))
      case BuildMapInstance(values) => {
        val newMapValues = for {
          (k, v) <- values
        } yield {
          val nps = Evaluator.newParametersFromPattern(k).toSet
          val newValue = this(v, parameters.filter(x => !nps.contains(x._1)), env)
          k -> newValue
        }

        BuildMapInstance(newMapValues)
      }
    }
  }

  // TODO - this will become unneccesary when we create a "buildMap" instead of relying on NewMapObject
  // NewMapObject should not contain any outside parameters!!!
  def substObject(
    nObject: UntaggedObject,
    parameters: Map[String, NewMapExpression],
    env: Environment
  ): UntaggedObject = {
    nObject match {
      case UMap(values) => {
        val newValues = for {
          (k, v) <- values

          internalParams = Evaluator.newParametersFromPattern(k)
          // We cannot replace params that have the same name

          remainingParameters = parameters -- internalParams
        } yield (k -> this(v, remainingParameters, env))

        UMap(newValues)
      }
      case _ => nObject
    }
  }
}