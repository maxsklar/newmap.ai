package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Subsitute the given parameters for their given values in the expression
object MakeSubstitution {
  def apply(
    expression: NewMapExpression,
    parameters: Map[String, UntaggedObject]
  ): NewMapExpression = {
    expression match {
      case ObjectExpression(nObject) => {
        // Temporary solution is to dig through to find the map expressions with the parameters
        // Permanent solution is to make a "build map construction + functions"
        val fixedObject = substObject(nObject, parameters)
        ObjectExpression(fixedObject)
      }
      case ApplyFunction(func, input) => {
        ApplyFunction(
          this(func, parameters),
          this(input, parameters)
        )
      }
      case ParamId(name) => {
        parameters.get(name) match {
          case Some(uObject) => ObjectExpression(uObject)
          case None => expression
        }
      }
      case BuildCase(constructor, input) => {
        BuildCase(constructor, this(input, parameters))
      }
      case BuildSimpleMapT(inputExp, outputExp, config) => {
        BuildSimpleMapT(
          this(inputExp, parameters),
          this(outputExp, parameters),
          config
        )
      }
      case BuildMapT(typeTransform, config) => {
        BuildMapT(this(typeTransform, parameters), config)
      }
      case BuildTableT(keyType, requiredValues) => {
        BuildTableT(
          this(keyType, parameters),
          this(requiredValues, parameters)
        )
      }
      case BuildSubtypeT(isMember, parentType, featureSet) => {
        BuildSubtypeT(this(isMember, parameters), this(parentType, parameters), featureSet)
      }
      case BuildCaseT(cases, parentFieldType, featureSet) => BuildCaseT(this(cases, parameters), parentFieldType, featureSet)
      case BuildStructT(params, parentFieldType, completeness, featureSet) => BuildStructT(this(params, parameters), parentFieldType, completeness, featureSet)
      case BuildNewTypeClassT(typeTransform) => BuildNewTypeClassT(this(typeTransform, parameters))
      case BuildMapInstance(values) => {
        val newMapValues = for {
          (k, v) <- values
        } yield {
          val nps = Evaluator.newParametersFromPattern(k).toSet
          val newValue = this(v, parameters.filter(x => !nps.contains(x._1)))
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
    parameters: Map[String, UntaggedObject]
  ): UntaggedObject = {
    nObject match {
      case UCase(constructor, values) => {
        UCase(constructor, substObject(values, parameters))
      }
      case UStruct(values) => {
        UStruct(values.map(v => substObject(v, parameters)))
      }
      case UParamId(name) => {
        parameters.get(name) match {
          case Some(nExpression) => nExpression
          case None => nObject
        }
      }
      case UMap(values) => {
        val newValues = for {
          (k, v) <- values

          internalParams = Evaluator.newParametersFromPattern(k)
          // We cannot replace params that have the same name

          remainingParameters = parameters -- internalParams
        } yield (k -> this(v, remainingParameters))

        UMap(newValues)
      }
      case _ => nObject
    }
  }
}