package ai.newmap.model

import java.util.UUID

sealed abstract class NewMapType {
  override def toString = PrintNewMapObject.newMapType(this, Environment.Base.typeSystem)
  def displayString(env: Environment) = PrintNewMapObject.newMapType(this, env.typeSystem)
}

/*
 * The types in the NewMap Language
 * This is actually a subset of the Objects
 */
case object CountT extends NewMapType

// The value of "i" should be a Count
case class IndexT(i: UntaggedObject) extends NewMapType

// Base types (will later be equated to derived/userdefined types)
case object BooleanT extends NewMapType
case object ByteT extends NewMapType
case object CharacterT extends NewMapType
//case object StringT extends NewMapType
case object LongT extends NewMapType
case object DoubleT extends NewMapType
case object UuidT extends NewMapType

case class WildcardPatternT(s: String) extends NewMapType
case class ParamIdT(s: String) extends NewMapType

// Type of types that exist in the given state
case class HistoricalTypeT(uuid: UUID) extends NewMapType

// Type of all types
case object TypeT extends NewMapType

// This is the "bottom" type
// It's equivalent to type 0 or type Case()
// Difference here is that we disallow any function to ever return this through the type checker. This is truly the "base type"
case object UndefinedT extends NewMapType

// Todo - replace with "user defined type" in prelude
case object IdentifierT extends NewMapType

/* Note on maps:
 *
 * A map is essentially a function or key-value store, and there are 3 levels:
 * - The first (BasicMap) is a map where each key-value pair needs to be specified directly.
 *   BasicMap has the distinction of always being finite!
 * - The second is a simple function, which can be coded but it's likely to be executed quickly
 *   (In other words, the coding set is not turing complete and infinite loops are avoided.
 *    we can thus execute simple functions without too much worry about compute time
 * - The third is the full function - which has a full coding set to turn an input into an output
 *
 * Note that a map can be used as a simple function and a full function,
 *  and a simple function can be used as a full function.
 *
 * The completeness field also has several options:
 * - requireAllFields tells us that we are required to specify an output for
 *   all potential inputs. It's smart to turn this on to ensure that functions and maps are checked as complete
 * - commandOutput means that the output types must all be command types, which means they start at an initial value.
 */
case class MapT(
  typeTransform: UntaggedObject, // Must be a PatternMap
  config: MapConfig
) extends NewMapType

case class MapConfig(
  completeness: MapCompleteness,
  featureSet: MapFeatureSet,
  preservationRules: Vector[PreservationRule] = Vector.empty,
  channels: Vector[(UntaggedObject, NewMapType)] = Vector.empty,
  channelParentType: NewMapType = IdentifierT
)

sealed abstract class PreservationRule {
  def toUntaggedObject: UntaggedObject = UStruct(Vector.empty)
}
// Preservation
// Preserving Equality: [a == b] == [f(a) == f(b)]
// First we need equality to be a generic type..

sealed abstract class MapCompleteness {
  def getName: String
}
object RequireCompleteness extends MapCompleteness {
  override def getName: String = "RequireCompleteness"
}

object CommandOutput extends MapCompleteness {
  override def getName: String = "CommandOutput"
}

// This is actually not a full map, but a pattern that may match a map
// The untagged object of this is going to be UMapPattern
object MapPattern extends MapCompleteness {
  override def getName: String = "MapPattern"
}

sealed abstract class MapFeatureSet {
  def getName: String
  def getLevel: Int
}

object BasicMap extends MapFeatureSet {
  override def getName: String = "BasicMap"
  override def getLevel: Int = 0
}

object PatternMap extends MapFeatureSet {
  override def getName: String = "PatternMap"
  override def getLevel: Int = 1
}

// Allows Pattern Matching, only simple operations
object SimpleFunction extends MapFeatureSet  {
  override def getName: String = "SimpleFunction"
  override def getLevel: Int = 2
}

// Allows recursion only if it provably simplifies the input
object WellFoundedFunction extends MapFeatureSet {
  override def getName: String = "WellFoundedFunction"
  override def getLevel: Int = 3
}

// Turing Complete - may sometimes go into an infinite loop
object FullFunction extends MapFeatureSet {
  override def getName: String = "FullFunction"
  override def getLevel: Int = 4
}

/*case class TypeParameter(
  name: String,
  upperBound: Vector[NewMapType] = Vector.empty,
  lowerBound: Vector[NewMapType] = Vector.empty,
  variance: Option[TypeParameterVariance] = None, // If none - then variance is inferred
)

//  How does this framework make sense in a Type Transformer world?
case class TypeParameterVariance(
  isCovariant: Boolean,
  isContravariant: Boolean
)*/

// Params should be connected to a NewMapObject which are of type
//  ReqMap(fieldType, TypeT)
// They might also be a paramObj, to be filled in later
// TODO(2022): once generics are introduced, fieldType might be unneccesary
// TODO: What about simpler product types (no identifiers) based on MapT(TypeT, Count, CommandOutput, BasicMap)

// Params is a map from the fields of the struct to the Types (all the same level)
case class StructT(
  params: Vector[(UntaggedObject, UntaggedObject)],
  fieldParentType: NewMapType,
  completeness: MapCompleteness = RequireCompleteness,
  featureSet: MapFeatureSet = BasicMap
) extends NewMapType

// Is this really a type? Or just an instance of SubtypeT(TypeT)?
case class TypeClassT(
  // The typeTransform encodes the abstract values that this type class is required to have.
  // Eg (t: t => String) means that every type in this class can calculate a String value for all it's objects.
  // Note that this value isn't named, but its'
  typeTransformPatter: UMapPattern,

  // List of the actual types in the class
  // Along with their implementation
  typesInTypeClass: Vector[(UntaggedObject, UntaggedObject)],
) extends NewMapType

// cases: input type is the case constructors, output type is the field types per constructor
// TODO - this is not really a type because of the existance of type parameters. This is more of a type constructor
case class CaseT(
  cases: Vector[(UntaggedObject, UntaggedObject)],
  fieldParentType: NewMapType,
  featureSet: MapFeatureSet = BasicMap,
) extends NewMapType

// Represents a type that contains a subset of the parent type, represented by a simple function
// - The output type of the simple function is usually a boolean (2) or at least a command type
// - Anything that's left at the initial value is NOT in the subtype
// For example, if the simple function is a Map from 10 to 2, and it reads (2: 1, 3: 1, 5: 1, 7: 1),
//  then the values 2, 3, 5, and 7 are considered part of this new type; the rest are not
case class SubtypeT(
  isMember: UntaggedObject,
  parentType: NewMapType,
  featureSet: MapFeatureSet = BasicMap
) extends NewMapType

/** 
 * This represents a "system" of functions that are potentially mutually recursive on one another.
 * The keys (the way we name these functions) are assumed to identifier, so no need to give it one.
 * The values are MapT type signatures
 */
case class FunctionalSystemT(
  // Contains the type of each function
  functionTypes: Vector[(UntaggedObject, UntaggedObject)]
) extends NewMapType

case class CustomT(
  name: String,
  params: UntaggedObject
) extends NewMapType

// Inside should be a UCase(Uuuid(id), t)
// where id is the type system id that created the type
// and t is the actual type
case class WithStateT(
  typeSystemId: UUID,
  nType: NewMapType 
) extends NewMapType