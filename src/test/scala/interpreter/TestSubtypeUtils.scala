package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

class TestSubtypeUtils extends FlatSpec {
  "Subtype " should " be convertible into the parent type" in {
    val isMember = UMap(Vector(
      UIdentifier("key") -> ObjectExpression(UIndex(1)),
      UIdentifier("value") -> ObjectExpression(UIndex(1))
    ))

    val convertInstructionOutcome = SubtypeUtils.isTypeConvertible(SubtypeT(isMember, IdentifierT, BasicMap), IdentifierT, Environment.Base)
    
    assert(convertInstructionOutcome.isSuccess)
  }
}