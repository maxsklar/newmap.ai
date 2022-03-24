package ai.newmap.model

import java.util.UUID

/*
 * The objects in the NewMap Language
 */
sealed abstract class NewMapObject {
  override def toString = PrintNewMapObject(this)
}

case class IdentifierInstance(s: String) extends NewMapObject

sealed abstract class NewMapPattern

// Idea: if MapT is an ordered map, or if it is a reqmap from an Index (array), then
//  we should be able to just give values and not keys if we provide the whole thing
case class MapInstance(
  values: Vector[(NewMapPattern, NewMapObject)],
  mapType: MapT
) extends NewMapObject

case class ObjectPattern(
  nObject: NewMapObject
) extends NewMapPattern

case class TypePattern(
  name: String,
  nType: NewMapObject
) extends NewMapPattern

case class StructPattern(
  params: Vector[NewMapPattern]
) extends NewMapPattern

case class ApplyFunction(
  func: NewMapObject,
  input: NewMapObject
) extends NewMapObject

case class AccessField(
  struct: NewMapObject, // This must be an object that has fields (StructInstance, CaseT)
  input: NewMapObject // Note - input must be literal and free of parameters (might be able to do this with scala's type systen)
) extends NewMapObject

// This is an object that stands for something else in the environment
// Very important so that we don't repeat code
// TODO - perhaps in the future, every NewMapObject will actually be a pointer to a hash table, and these will be irrelevant
case class ParamId(name: String) extends NewMapObject

// This is a parameter that is a standin
case class ParameterObj(uuid: UUID, nType: NewMapObject) extends NewMapObject

// This takes as input a member of TypeT and returns true if it's a member
//  of the command typeclass (which means it has a default value and an update function)
// TODO: making this a basic class is temporary for now
case object IsCommandFunc extends NewMapObject

// Temporary for now, this should be buildable as a map pattern in the future
case object IsSimpleFunction extends NewMapObject

case object IsVersionedFunc extends NewMapObject

// A basic function to increment a count
case object IncrementFunc extends NewMapObject

// This one is a little different/complex because each object has a unique type as defined by the struct
// TODO: should we merge this with MapInstance, since a type is going to be attached anyway!
// - That could cause problems because maps don't have to be finite
// - Then again, an infinite struct could open up possibilities!!
// The input NewMapObject values must be closed and evaluated
case class StructInstance(value: Vector[(NewMapPattern, NewMapObject)], structType: StructT) extends NewMapObject

case class CaseInstance(constructor: NewMapObject, input: NewMapObject, caseType: CaseT) extends NewMapObject

// Note that this should eventually be turned into a ReqMap(Index(n), output)
// But because we don't know "n" we can make this its own type for now
case class SequenceT(underlyingType: NewMapObject) extends NewMapObject

case class SequenceInstance(seq: Vector[NewMapObject], sequenceT: SequenceT) extends NewMapObject

// Basic Function Section
// These are pre-defined functions, their types are in comment

/*
 * The types in the NewMap Language
 * This is actually a subset of the Objects
 */
case object CountT extends NewMapObject

case class Index(i: Long) extends NewMapObject

// This is actually not a type!
case class IndexValue(i: Long, fromType: NewMapObject) extends NewMapObject

// Type of types
// TODO - eventually, we will replace this with an IsType function
//  that will be a subtype of type object
case object TypeT extends NewMapObject

case object AnyT extends NewMapObject

case object IdentifierT extends NewMapObject

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
  inputType: NewMapObject,
  outputType: NewMapObject,
  completeness: MapCompleteness,
  featureSet: MapFeatureSet
) extends NewMapObject

sealed abstract class MapCompleteness
object RequireCompleteness extends MapCompleteness
object CommandOutput extends MapCompleteness
object SubtypeInput extends MapCompleteness

sealed abstract class MapFeatureSet
object BasicMap extends MapFeatureSet
object SimpleFunction extends MapFeatureSet
object FullFunction extends MapFeatureSet


// Params should be connected to a NewMapObject which are of type
//  MapT(fieldType, TypeT, RequireCompleteness, SimpleFunction)
// They might also be a paramObj, to be filled in later
// TODO(2022): once generics are introduced, fieldType might be unneccesary
// TODO: What about simpler product types (no identifiers) based on MapT(TypeT, Count, CommandOutput, BasicMap)

// Params is a map from the fields of the struct to the Types (all the same level)
case class StructT(params: NewMapObject) extends NewMapObject

// cases: input type is the case constructors, output type is the field types per constructor
case class CaseT(cases: NewMapObject) extends NewMapObject

// Represents a type that contains a subset of the parent type, represented by a simple function
// - The output type of the simple function is usually a boolean (2) or at least a command type
// - Anything that's left at the initial value is NOT in the subtype
// For example, if the simple function is a Map from 10 to 2, and it reads (2: 1, 3: 1, 5: 1, 7: 1),
//  then the values 2, 3, 5, and 7 are considered part of this new type; the rest are not
case class SubtypeT(
  isMember: NewMapObject
) extends NewMapObject

// The versionNumber and uuid uniquely define this versioned object within any environment
// (of course different environments might have updated the object differently)
case class VersionedObject(
  currentState: NewMapObject,
  commandType: NewMapObject,
  versionNumber: Long
) extends NewMapObject

//def uuid = java.util.UUID.randomUUID

/*
case class TypeParameter(
  name: String,
  upperBounds: Vector[NewMapObject] = Vector.empty,
  lowerBounds: Vector[NewMapObject] = Vector.empty,
  variance: Option[TypeParameterVariance] = None, // If none - then variance is inferred
)

case class TypeParameterVariance(
  isCovariant: Boolean,
  isContravariant: Boolean
)*/

object NewMapO {
  // This is a subtype of TypeT, basically a newmap object with a command structure
  // - It has an initial value
  // - It has a command type
  // - You can give it commands to change the value
  // - You can potentially have versions available.
  def commandT: NewMapObject = SubtypeT(IsCommandFunc)

  // This is a subtype of any, and will match every map that is a simple function (or basicMap)
  // - This will be replaced once we get Map Type patterns working properly
  // - Created for now to get Subtype working properly, so that we can move on
  def simpleFunctionT: NewMapObject = SubtypeT(IsSimpleFunction)

  def versionedT: NewMapObject = SubtypeT(IsVersionedFunc)

  def emptyStruct: NewMapObject = StructT(
    MapInstance(Vector.empty, MapT(Index(0), Index(0), RequireCompleteness, BasicMap))
  )
}