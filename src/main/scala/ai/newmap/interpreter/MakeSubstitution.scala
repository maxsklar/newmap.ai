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
      case ParamId(name) => {
        parameters.get(name) match {
          case Some(uObject) => uObject
          case None => expression
        }
      }
      case UCase(constructor, input) => {
        UCase(constructor, this(input, parameters))
      }
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
}