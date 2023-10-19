package ai.newmap.interpreter

import ai.newmap.model._

// Substitute the given parameters for their given values in the expression
object MakeSubstitution {
  def apply(
    expression: UntaggedObject,
    parameters: Map[String, UntaggedObject]
  ): UntaggedObject = {
    expression match {
      case ApplyFunction(func, input, matchingRules) => {
        ApplyFunction(
          this(func, parameters),
          this(input, parameters),
          matchingRules
        )
      }
      case ParamId(name) => parameters.get(name).getOrElse(expression)
      case UCase(constructor, input) => {
        UCase(constructor, this(input, parameters))
      }
      case UMap(values) => {
        val newMapValues = for {
          (k, v) <- values
        } yield {
          val nps = Evaluator.newParametersFromPattern(k).toSet
          val newValue = this(v, parameters.filter(x => !nps.contains(x._1)))

          // Note that UMap keys should NOT contain parameters.
          // - This is because a map cannot be organized into a data structure if the keys are unknown
          // - If there needs to be a parameters in the key, use the single-pair version of UMap, which is USingularMap
          k -> newValue
        }

        UMap(newMapValues)
      }
      case USingularMap(k, v) => {
        val newKey = this(k, parameters)
        val nps = Evaluator.newParametersFromPattern(k).toSet
        val newValue = this(v, parameters.filter(x => !nps.contains(x._1)))
        USingularMap(newKey, newValue)
      }
      case UArray(values) => UArray(values.map(v => this(v, parameters)))
      case AccessField(value: UntaggedObject, uTypeClass: UntaggedObject, field: UntaggedObject) => {
        AccessField(
          this(value, parameters),
          uTypeClass, // TODO: do we do a substitution here?
          field // TODO: do we do a substitution here?
        )
      }
      case _ => expression
    }
  }
}