package ai.newmap.model

/*
 * The objects in the NewMap Language
 */
sealed abstract class NewMapObject {
  override def toString = PrintNewMapObject(this)
}

case class IdentifierInstance(s: String) extends NewMapObject

case class Index(i: Long) extends NewMapObject

// Idea: if MapT is an ordered map, or if it is a reqmap from an Index (array), then
//  we should be able to just give values and not keys if we provide the whole thing
case class MapInstance(
  values: Vector[(NewMapObject, NewMapObject)],
  mapType: MapT
) extends NewMapObject

// When mattern matching, we want to have a case where we want to know if this pattern equals this type
//case class AsTypePattern(pattern: NewMapObject, nType: NewMapObject) extends NewMapObject

// Different from a hard identifier, this establishes variable in a pattern match
case class IdentifierPattern(name: String, nType: NewMapObject) extends NewMapObject


// Perhaps redo this - rebrand as a special case of mapInstance! (crazy idea)
// The input NewMapObject values must be closed and evaluated
case class LambdaInstance(
  params: Vector[(NewMapObject, NewMapObject)],
  expression: NewMapObject
) extends NewMapObject

case class ApplyFunction(
  func: NewMapObject,
  input: NewMapObject
) extends NewMapObject

case class AccessField(
  struct: NewMapObject, // This must be an object that has fields (StructInstance, CaseT)
  input: NewMapObject // Note - input must be literal and free of parameters (might be able to do this with scala's type systen)
) extends NewMapObject

// This is an object that we don't know the value of yet. It'll be passed in later.
case class ParameterObj(name: String, nType: NewMapObject) extends NewMapObject

// This function takes a count and returns true if that count is strictly less than i
case class RangeFunc(i: Long) extends NewMapObject

// This takes as input a member of TypeT and returns true if it's a member
//  of the command typeclass (which means it has a default value and an update function)
// TODO: making this a basic class is temporary for now
case object IsCommandFunc extends NewMapObject

// A basic function to increment a count
case object IncrementFunc extends NewMapObject

// This one is a little different/complex because each object has a unique type as defined by the struct
// TODO: should we merge this with MapInstance, since a type is going to be attached anyway!
// - That could cause problems because maps don't have to be finite
// - Then again, an infinite struct could open up possibilities!!
// The input NewMapObject values must be closed and evaluated
case class StructInstance(value: Vector[(NewMapObject, NewMapObject)], structType: StructT) extends NewMapObject

case class CaseInstance(constructor: NewMapObject, input: NewMapObject, caseType: CaseT) extends NewMapObject

// Basic Function Section
// These are pre-defined functions, their types are in comment

/*
 * The types in the NewMap Language
 * This is actually a subset of the Objects
 */

case object CountT extends NewMapObject

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

// This is a custom type of nodes that are unique
/*case class CustomT(
  uuid: String,
  ordered: Boolean,
  items: Vector[NewMapObject] // could there be uuids in here? Any other possibilities 
)*/

//def uuid = java.util.UUID.randomUUID.toString


/*case class MutableObject(
  version: Long,
  nCurrent: NewMapObject,
  nType: NewMapObject // This is in IsCommandFunc = true
) extends NewMapObject*/

// Figure this out!
/*case class GenericType(
  parameters: Vector[(String, TypeParameter)],
  body: NewMapObject
) extends NewMapObject

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
  
  def rangeT(i: Long): NewMapObject = SubtypeT(RangeFunc(i))

  // This is a subtype of TypeT, basically a newmap object with a command structure
  // - It has an initial value
  // - It has a command type
  // - You can give it commands to change the value
  // - You can potentially have versions available.
  def commandT: NewMapObject = SubtypeT(IsCommandFunc)

  // For now, store a sequence as a map
  // TODO - this will get more efficient later on!
  def smallFiniteSequence(items: Vector[NewMapObject], nType: NewMapObject): NewMapObject = {
    MapInstance(
      items.zipWithIndex.map(x => Index(x._2) -> x._1),
      MapT(rangeT(items.length), nType, RequireCompleteness, BasicMap)
    )
  }
}