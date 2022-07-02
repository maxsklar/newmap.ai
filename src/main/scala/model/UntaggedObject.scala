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

case class UMap(values: Vector[(UntaggedObject, UntaggedObject)]) extends UntaggedObject

// This is equivalent to UMap where the keys are indecies
case class UStruct(values: Vector[UntaggedObject]) extends UntaggedObject

// Represents any object (of a given type)
case class UWildcardPattern(s: String) extends UntaggedObject

// Represents a parametrized object (this can only exist in environments with parameters)
case class ParamId(s: String) extends UntaggedObject

// This can't be a map/struct because here the type of the input depends on the constructor
case class UCase(constructor: UntaggedObject, input: UntaggedObject) extends UntaggedObject

case class UIndex(i: Long) extends UntaggedObject

case class ULet(commands: Vector[EnvironmentCommand], output: UntaggedObject) extends UntaggedObject

// Set to the initial value of a command type
case object UInit extends UntaggedObject
case class ULink(key: VersionedObjectKey) extends UntaggedObject

// To be moved into user-defined types soon
case class UByte(value: Byte) extends UntaggedObject
case class UCharacter(value: Char) extends UntaggedObject
//case class UString(length: Long, value: String) extends UntaggedObject
case class ULong(value: Long) extends UntaggedObject
case class UDouble(value: Double) extends UntaggedObject
case class Uuuid(value: UUID) extends UntaggedObject

// Built in functions

// This takes as input a member of TypeT and returns true if it's a member
//  of the command typeclass (which means it has a default value and an update function)
// TODO: making this a basic class is temporary for now
case object IsCommandFunc extends UntaggedObject

// Only for Expressions
case class ApplyFunction(
  func: UntaggedObject,
  input: UntaggedObject,
  matchingRules: MatchingRules // See Class Comment
) extends UntaggedObject

case class FunctionWithMatchingRules(
  func: UntaggedObject,
  matchingRules: MatchingRules
)

/*
 * This represents our ability to change how the pattern matching algorithm works
 * depending on the type of data that's being matched.
 * In the future, this will be represented by a relation within the newmap system (as a newmap object)
 * - And it'll be required to be a transitive relation
 */
sealed abstract class MatchingRules
object StandardMatcher extends MatchingRules
object TypeMatcher extends MatchingRules