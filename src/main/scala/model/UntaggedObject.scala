package ai.newmap.model

import java.util.UUID

/*
 * The objects in the NewMap Language
 */
sealed abstract class UntaggedObject {
  override def toString = PrintNewMapObject.untagged(this)
}

// Todo - replace with "user defined type" in prelude
case class UIdentifier(s: String) extends UntaggedObject

case class UMap(values: Vector[(NewMapPattern, NewMapExpression)]) extends UntaggedObject

// This can't be a map/struct because here the type of the input depends on the constructor
case class UCase(constructor: UntaggedObject, input: UntaggedObject) extends UntaggedObject

case class UIndex(i: Long) extends UntaggedObject

// Set to the initial value of a command type
case object UInit extends UntaggedObject

// We can't yet untag types directly, so we can use this instead
case class UType(nType: NewMapType) extends UntaggedObject

case class ULink(key: VersionedObjectKey) extends UntaggedObject

// To be moved into user-defined types soon
case class UByte(value: Byte) extends UntaggedObject
case class UCharacter(value: Char) extends UntaggedObject
case class UString(value: String) extends UntaggedObject
case class ULong(value: Long) extends UntaggedObject
case class UDouble(value: Double) extends UntaggedObject


// Note this is actually an UntaggedObject with type MapT(??, TypeT, RequireCompleteness, SimpleFunction)
// But it's really more associated with being a type
case class UParametrizedCaseT(
  parameters: Vector[(String, NewMapType)],
  caseT: CaseT // This must be an expression because it has parameters
) extends UntaggedObject

// Built in functions

// This takes as input a member of TypeT and returns true if it's a member
//  of the command typeclass (which means it has a default value and an update function)
// TODO: making this a basic class is temporary for now
case object IsCommandFunc extends UntaggedObject

// A basic function to increment a count
case object IncrementFunc extends UntaggedObject