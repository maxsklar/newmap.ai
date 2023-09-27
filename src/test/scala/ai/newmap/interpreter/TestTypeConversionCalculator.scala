package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._

class TestTypeConversionCalculator extends FlatSpec {
  "Subtype " should " be convertible into the parent type" in {
    val isMember = UMap(Vector(
      UIdentifier("key") -> UIndex(1),
      UIdentifier("value") -> UIndex(1)
    ))

    val convertInstructionOutcome = TypeConversionCalculator.isTypeConvertible(SubtypeT(isMember, IdentifierT, BasicMap), IdentifierT, Environment.Base)
    
    assert(convertInstructionOutcome.isSuccess)
  }
}