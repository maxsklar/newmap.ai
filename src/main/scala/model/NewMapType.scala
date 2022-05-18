package ai.newmap.model

import java.util.UUID

sealed abstract class NewMapType {
  override def toString = PrintNewMapObject.newMapType(this)
}

/*
 * The types in the NewMap Language
 * This is actually a subset of the Objects
 */
case object CountT extends NewMapType

case class IndexT(i: Long) extends NewMapType

// Base types (will later be equated to derived/userdefined types)
case object BooleanT extends NewMapType
case object ByteT extends NewMapType
case object CharacterT extends NewMapType
case object StringT extends NewMapType
case object LongT extends NewMapType
case object DoubleT extends NewMapType

// Type of types!
case object TypeT extends NewMapType

// This is the "bottom" type
// It's equivalent to type 0 or type Case()
// Difference here is that we disallow any function to ever return this. This is truly the "base type"
case object UndefinedT extends NewMapType

//case object AnyT extends NewMapType

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
 *
 * TODO - should we subsume struct type in here??
 */
case class MapT(
  inputType: NewMapType,
  outputType: NewMapType,
  config: MapConfig
) extends NewMapType

case class GenericMapT(
  typeTransform: Vector[(NewMapPattern, NewMapExpression)],
  config: MapConfig
) extends NewMapType

case class MapConfig(
  completeness: MapCompleteness,
  featureSet: MapFeatureSet,
  preservationRules: Vector[PreservationRule] = Vector.empty
)

sealed abstract class PreservationRule
// Preservation
// Preserving Equality: [a == b] == [f(a) == f(b)]
// First we need equality to be a generic type..

sealed abstract class MapCompleteness
object RequireCompleteness extends MapCompleteness
object CommandOutput extends MapCompleteness

sealed abstract class MapFeatureSet
object BasicMap extends MapFeatureSet
object SimpleFunction extends MapFeatureSet // Allows Pattern Matching, only simple operations
object WellFoundedFunction extends MapFeatureSet // Allows recursion only if it provably simplifies the input
object FullFunction extends MapFeatureSet // Turing Complete - may sometimes go into an infinite loop

case class TypeParameter(
  name: String,
  upperBound: Vector[NewMapType] = Vector.empty,
  lowerBound: Vector[NewMapType] = Vector.empty,
  variance: Option[TypeParameterVariance] = None, // If none - then variance is inferred
)

case class TypeParameterVariance(
  isCovariant: Boolean,
  isContravariant: Boolean
)

// Params should be connected to a NewMapObject which are of type
//  MapT(fieldType, TypeT, RequireCompleteness, SimpleFunction)
// They might also be a paramObj, to be filled in later
// TODO(2022): once generics are introduced, fieldType might be unneccesary
// TODO: What about simpler product types (no identifiers) based on MapT(TypeT, Count, CommandOutput, BasicMap)

// Params is a map from the fields of the struct to the Types (all the same level)
// TODO: We also need a feature set!
case class StructT(
  params: Vector[(NewMapPattern, NewMapExpression)],
  fieldParentType: NewMapType,
  completeness: MapCompleteness = RequireCompleteness,
  featureSet: MapFeatureSet = BasicMap
) extends NewMapType

// Is this really a type? Or just an instance of SubtypeT(TypeT)?
case class TypeClassT(
  // The typeTransform encodes the abstract values that this type class is required to have.
  // Eg (t: t => String) means that every type in this class can calculate a String value for all it's objects.
  // Note that this value isn't named, but its'
  typeTransform: Vector[(NewMapPattern, NewMapExpression)],

  // List of the actual types in the class, but not their implementations - that's in the actual object (with a UMap)
  typesInTypeClass: Vector[NewMapPattern],
) extends NewMapType

// cases: input type is the case constructors, output type is the field types per constructor
// TODO - this is not really a type because of the existance of type parameters. This is more of a type constructor
case class CaseT(
  cases: Vector[(NewMapPattern, NewMapExpression)],
  fieldParentType: NewMapType,
  featureSet: MapFeatureSet = BasicMap,
) extends NewMapType

case class ConstructedType(
  genericType: NewMapObject,
  params: UntaggedObject
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

case class CustomT(
  uuid: UUID, // Should I instead have a name here??
  nType: NewMapType
) extends NewMapType