package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

/**
 * This is the custom type convertibility graph
 * TODO: this can be a lot more complex!
 */
case class TypeConvertibilityGraph(
  graph: Map[NewMapType, Map[NewMapType, TypeConvertionResponse]] = Map.empty
) {
  def findPotentialConversions(nType: NewMapType): Map[NewMapType, TypeConvertionResponse] = {
    graph.get(nType).getOrElse(Map.empty)
  }

  // Run a breadth-first search
  def findAllConvertions(nType: NewMapType): Map[NewMapType, TypeConvertionResponse] = {
    // TODO: implement this so that it goes achieves depth
    findPotentialConversions(nType)
  }

  def addConversion(
    nTypeFrom: NewMapType,
    nTypeTo: NewMapType,
    convertFunction: FunctionWithMatchingRules
  ): TypeConvertibilityGraph = {

    // TODO: In reality, we're going to have to do a standard match using the evaluator
    val currentTypeConversions = graph.get(nTypeFrom).getOrElse(Map.empty)

    // TODO: more thought needs to go into this response (parameters, refinedEndingType, etc)
    val typeConvertionResponse = TypeConvertionResponse(Vector(convertFunction), nTypeTo)

    // What if there's an existing conversion? How do we handle this?
    val newTypeConversions = currentTypeConversions + (nTypeTo -> typeConvertionResponse)

    val newGraph = graph + (nTypeFrom -> newTypeConversions)

    TypeConvertibilityGraph(newGraph)
  }
}

object TypeConvertibilityGraph {
  val init: TypeConvertibilityGraph = {
    TypeConvertibilityGraph()
      .addConversion(CountT, DoubleT, FunctionWithMatchingRules(UCountToDecimal, StandardMatcher))
  }
}