package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

class TestSubtypeUtils extends FlatSpec {
  "Subtype " should " be convertible into the parent type" in {
    val isMember = TaggedObject(
      UMap(Vector(
        ObjectPattern(UIdentifier("key")) -> ObjectExpression(TaggedObject(UIndex(1), TaggedObject(UIndex(2), CountT))),
        ObjectPattern(UIdentifier("value")) -> ObjectExpression(TaggedObject(UIndex(1), TaggedObject(UIndex(2), CountT)))
      )),
      MapT(IdentifierT, TaggedObject(UIndex(2), CountT), CommandOutput, BasicMap)
    )

    assert(RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(isMember), Environment.Base) == IdentifierT)

    val convertInstructionOutcome = SubtypeUtils.isTypeConvertible(SubtypeT(isMember), IdentifierT, Environment.Base)
    
    assert(convertInstructionOutcome.isSuccess)
  }
}