package ai.newmap.model

/**
 * The patterns in the NewMapLanguage
 */
sealed abstract class NewMapPattern

case class ObjectPattern(
  uObject: UntaggedObject
) extends NewMapPattern

// Replace ObjectPattern with other patterns


case class WildcardPattern(
  name: String
) extends NewMapPattern

case class StructPattern(
  params: Vector[NewMapPattern]
) extends NewMapPattern

case class CasePattern(
  constructor: UntaggedObject,
  input: NewMapPattern
) extends NewMapPattern


// This is a temporary pattern until types are stored as cases, in which case we'll use casePattern
case class MapTPattern(
  input: NewMapPattern,
  outputType: NewMapPattern,
  config: MapConfig
) extends NewMapPattern


/*
case IdentifierPattern(s: String) extends NewMapPattern
case BytePattern(value: Byte) extends NewMapPattern
case CharacterPattern(value: Char) extends NewMapPattern
case StringPattern(value: String) extends NewMapPattern
case LongPattern(value: Long) extends NewMapPattern
case DoublePattern(value: Double) extends NewMapPattern

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

*/