package ai.newmap.interpreter

import ai.newmap.model._

// Substitute the given parameters for their given values in the expression
object MakeSubstitution {
  def apply(
    expression: UntaggedObject,
    parameters: Map[String, UntaggedObject],

    // TODO: Once we remove wildcards and banish parameters from map keys, this can be removed
    includeWildcards: Boolean = false
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
      case UWildcardPattern(name) => {
        // TODO: Once we banish parameters from map keys, this can be removed
        if (includeWildcards) {
          parameters.get(name).getOrElse(expression)
        } else expression
      }
      case UCase(constructor, input) => {
        UCase(constructor, this(input, parameters))
      }
      case UMap(values) => {
        val newMapValues = for {
          (k, v) <- values
        } yield {
          // Note that Map Keys SHOULD NOT contain parameters.
          // Figure out why we need to do this here.
          val newKey = this(k, parameters)

          // I'm pretty sure that this is supposed to be "k" and not "newkey" at this point
          // - I'm having a hard time articulating why!
          val nps = Evaluator.newParametersFromPattern(k).toSet
          val newValue = this(v, parameters.filter(x => !nps.contains(x._1)))

          newKey -> newValue
          // Eventually, we should have this unless it's a singleton map
          //k -> newValue
        }

        UMap(newMapValues)
      }
      case UStruct(values) => UStruct(values.map(v => this(v, parameters)))
      case _ => expression
    }
  }
}