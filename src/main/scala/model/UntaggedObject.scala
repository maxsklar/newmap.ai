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

case class UMap(values: Vector[(UntaggedObject, NewMapExpression)]) extends UntaggedObject

// This is equivalent to UMap where the keys are indecies
case class UStruct(values: Vector[UntaggedObject]) extends UntaggedObject

// Represents any object (of a given type)
case class UWildcardPattern(s: String) extends UntaggedObject

// Represents a parametrized object (this can only exist in environments with parameters)
case class UParamId(s: String) extends UntaggedObject

// This can't be a map/struct because here the type of the input depends on the constructor
case class UCase(constructor: UntaggedObject, input: UntaggedObject) extends UntaggedObject

case class UIndex(i: Long) extends UntaggedObject

// Set to the initial value of a command type
case object UInit extends UntaggedObject

case class ULink(key: VersionedObjectKey) extends UntaggedObject

// To be moved into user-defined types soon
case class UByte(value: Byte) extends UntaggedObject
case class UCharacter(value: Char) extends UntaggedObject
case class UString(value: String) extends UntaggedObject
case class ULong(value: Long) extends UntaggedObject
case class UDouble(value: Double) extends UntaggedObject
case class Uuuid(value: UUID) extends UntaggedObject

// Built in functions

// This takes as input a member of TypeT and returns true if it's a member
//  of the command typeclass (which means it has a default value and an update function)
// TODO: making this a basic class is temporary for now
case object IsCommandFunc extends UntaggedObject

// A basic function to increment a count
case object IncrementFunc extends UntaggedObject