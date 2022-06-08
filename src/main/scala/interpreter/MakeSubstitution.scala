package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Subsitute the given parameters for their given values in the expression
object MakeSubstitution {
  def apply(
    expression: UntaggedObject,
    parameters: Map[String, UntaggedObject]
  ): UntaggedObject = {
    expression match {
      /*case ObjectExpression(nObject) => {
        // Temporary solution is to dig through to find the map expressions with the parameters
        // Permanent solution is to make a "build map construction + functions"
        val fixedObject = substObject(nObject, parameters)
        ObjectExpression(fixedObject)
      }*/
      case ApplyFunction(func, input) => {
        ApplyFunction(
          this(func, parameters),
          this(input, parameters)
        )
      }
      case ParamId(name) => {
        parameters.get(name) match {
          case Some(uObject) => uObject
          case None => expression
        }
      }
      case UCase(constructor, input) => {
        UCase(constructor, this(input, parameters))
      }
      /*case BuildSimpleMapT(inputExp, outputExp, config) => {
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
      case UCaseT(cases, parentFieldType, featureSet) => UCaseT(this(cases, parameters), parentFieldType, featureSet)
      case BuildStructT(params, parentFieldType, completeness, featureSet) => BuildStructT(this(params, parameters), parentFieldType, completeness, featureSet)
      case BuildNewTypeClassT(typeTransform) => BuildNewTypeClassT(this(typeTransform, parameters))*/
      case UMap(values) => {
        val newMapValues = for {
          (k, v) <- values
        } yield {
          val newKey = this(k, parameters)

          // I'm pretty sure that this is supposed to be "k" and not "newkey" at this point
          // - I'm having a hard time articulating why!
          val nps = Evaluator.newParametersFromPattern(k).toSet
          val newValue = this(v, parameters.filter(x => !nps.contains(x._1)))
          newKey -> newValue
        }

        UMap(newMapValues)
      }
      case UStruct(values) => UStruct(values.map(v => this(v, parameters)))
      case constant => constant
    }
  }

  // TODO - this will become unneccesary when we create a "buildMap" instead of relying on NewMapObject
  // NewMapObject should not contain any outside parameters!!!
  /*def substObject(
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
      case ParamId(name) => {
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
  }*/
}