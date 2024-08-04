package ai.newmap.model

import java.util.UUID
import ai.newmap.util.{Outcome, Success, Failure}

/*
 * The objects in the NewMap Language
 */
sealed abstract class UntaggedObject {
  override def toString = PrintNewMapObject.untagged(this)

  def getMapBindings(): Outcome[Vector[(UntaggedObject, UntaggedObject)], String] = this match {
    case UMap(values) => Success(values)
    case USingularMap(key, value) => Success(Vector(key -> value))
    case UArray(values) => Success(values.zipWithIndex.map(x => UIndex(x._2) -> x._1).toVector)
    case UInit => Success(Vector.empty)
    case _ => Failure("Could not get bindings: " + this)
  }

  def asType: Outcome[NewMapType, String] = NewMapType.convertToNewMapType(this)
}

// Todo - replace with "user defined type" in prelude
case class UIdentifier(s: String) extends UntaggedObject

// TODO: change from Vector to ListMap
case class UMap(values: Vector[(UntaggedObject, UntaggedObject)]) extends UntaggedObject

object UMap {
  def apply(values: (UntaggedObject, UntaggedObject)*): UMap = {
    UMap(values.toVector)
  }
}

// This is equivalent to UMap where the keys are indecies
case class UArray(values: Array[UntaggedObject]) extends UntaggedObject

// Make it easer to create values
object UArray {
  def apply(values: UntaggedObject*): UArray = new UArray(values.toArray)
}

// Represents any object (of a given type)
case class UWildcard(s: String) extends UntaggedObject

case class USingularMap(key: UntaggedObject, value: UntaggedObject) extends UntaggedObject

// This is a pattern that matches a map.
// It first looks to see if the keyPattern is represented in the map, and if it is, does it match the value pattern
//case class USingularMapPattern(key: UntaggedObject, value: UntaggedObject)

// Represents a parametrized object (this can only exist in environments with parameters)
case class ParamId(s: String) extends UntaggedObject

// This can't be a map/struct because here the type of the input depends on the constructor
case class UCase(constructor: UntaggedObject, input: UntaggedObject) extends UntaggedObject

case class UIndex(i: Long) extends UntaggedObject

case class ULet(commands: Vector[EnvironmentCommand], output: UntaggedObject) extends UntaggedObject

// Set to the initial value of a command type
case object UInit extends UntaggedObject

// The versionNumber and uuid uniquely define this versioned object within any environment
// (of course different environments might have updated the object differently)
case class VersionedObjectKey(
  versionNumber: Long,
  uuid: UUID
)

case class ULink(key: VersionedObjectKey) extends UntaggedObject

// To be moved into user-defined types soon
case class UByte(value: Byte) extends UntaggedObject
case class UCharacter(value: Char) extends UntaggedObject
case class ULong(value: Long) extends UntaggedObject
case class UDouble(value: Double) extends UntaggedObject
case class Uuuid(value: UUID) extends UntaggedObject

// Built in functions

// This takes as input a member of TypeT and returns true if it's a member
//  of the command typeclass (which means it has a default value and an update function)
// TODO: making this a basic class is temporary for now
case object IsCommandFunc extends UntaggedObject

// This takes a pair of numbers are returns their sum
case object UPlus extends UntaggedObject
case object UMinus extends UntaggedObject
case object UTimes extends UntaggedObject
case object UDivide extends UntaggedObject

// More Numerical Operations
case object UCountToDecimal extends UntaggedObject

// Only for Expressions
case class ApplyFunction(
  func: UntaggedObject,
  input: UntaggedObject,
  matchingRules: MatchingRules // See Class Comment
) extends UntaggedObject

case class AccessField(
  value: UntaggedObject,
  // TODO(max): we might not actually need the full type class here - but we do need some indicator of how to get the field from the value.
  // - Rework how this is done, or confirm that this is the best way to do it.
  uTypeClass: UntaggedObject,
  field: UntaggedObject
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