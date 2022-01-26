package ai.newmap.model

/*
 * The objects in the NewMap Language
 */
sealed abstract class NewMapObject {
  override def toString = PrintNewMapObject(this)
}

// These objects accept inputs as well as outputs
sealed abstract class NewMapFunction extends NewMapObject

case class IdentifierInstance(s: String) extends NewMapObject

case class Index(i: Long) extends NewMapObject

// Idea: create a MapInstance object that only has values and does not extend NewMapObject
// - Then, combine it with other fields in other objects to create what we want
case class MapInstance(
  values: Vector[(NewMapObject, NewMapObject)],
  mapType: MapT
) extends NewMapFunction

// There are several different ways items can be passed into a lambda expression
sealed abstract class LambdaParamStrategy

// The parameter type is a struct, and the name of the parameters is how the values are called
case class StructParams(params: Vector[(String, NewMapSubtype)]) extends LambdaParamStrategy

// The parameter is named by an identifier
case class IdentifierParam(name: String, nType: NewMapSubtype) extends LambdaParamStrategy

case class LambdaInstance(
  paramStrategy: LambdaParamStrategy,
  expression: NewMapObject
) extends NewMapFunction

case class ApplyFunction(
  func: NewMapFunction,
  input: NewMapObject
) extends NewMapObject

case class AccessField(
  struct: NewMapObject, // Todo - struct this is either a struct instance, or a case type
  input: NewMapObject // Note - input must be literal and free of parameters (might be able to do this with scala's type systen)
) extends NewMapObject

// This is an object that we don't know the value of yet. It'll be passed in later.
case class ParameterObj(name: String, nType: NewMapSubtype) extends NewMapObject

// TODO Might need more information in here -- like Completeness or featureset... same as MapInstance??
case class ParameterFunc(name: String, inputType: NewMapSubtype, outputType: NewMapSubtype) extends NewMapFunction

// This function takes a count and returns true if that count is strictly less than i
case class RangeFunc(i: Long) extends NewMapFunction

// This takes as input a member of TypeT(depth) and returns true if it's a member
//  of the command typeclass (which means it has a default value and an update function)
// TODO: making this a basic class is temporary for now
case class IsCommandFunc(depth: Long = 0) extends NewMapFunction

// This one is a little different/complex because each object has a unique type as defined by the struct
// TODO: should we merge this with MapInstance, since a type is going to be attached anyway!
// TODO(2022): the first value should be "NewMapObject" instead of string, but this might hurt the
//  type-checker/evaluator for now - change soon!
// TODO - revive this version!!
//case class StructInstance(value: Vector[(NewMapObject, NewMapObject)], structType: StructT) extends NewMapObject
case class StructInstance(value: Vector[(String, NewMapObject)], structType: StructT) extends NewMapObject

case class CaseInstance(constructor: NewMapObject, input: NewMapObject, caseType: CaseT) extends NewMapObject

// The type inputs a case class and outputs a type - so for each case it creates a function to that type
// Generally of type Case(params) => T
//case class MatchInstance(
//  patterns: NewMapObject // This must be of type MapType(C, T, default)
  // Where C: the case type that this refers to
  // T: the output type
  // default: the "else" clause
//)

// Basic Function Section
// These are pre-defined functions, their types are in comment

/*
 * The types in the NewMap Language
 * This is actually a subset of the Objects, and there are functions below that convert between the 2
 */
sealed abstract class NewMapSubtype extends NewMapObject
sealed abstract class NewMapType extends NewMapSubtype

case object CountT extends NewMapType

// Type of types.. very confusing
// The depthOfMembers input tells us which layer we are on.
// - Layer 0 is all of the ordinary types whose members are objects
// - Layer 1 is all of types of layer 0
// - Layer 2 is all of the types of layer 1, etc
case class TypeT(depthOfMembers: Long = 0) extends NewMapType

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
  inputType: NewMapSubtype,
  outputType: NewMapSubtype,
  completeness: MapCompleteness,
  featureSet: MapFeatureSet
) extends NewMapType

sealed abstract class MapCompleteness
object RequireCompleteness extends MapCompleteness
object CommandOutput extends MapCompleteness

sealed abstract class MapFeatureSet
object BasicMap extends MapFeatureSet
object SimpleFunction extends MapFeatureSet
object FullFunction extends MapFeatureSet


// Params should be connected to a NewMapObject which are of type
//  MapT(fieldType, TypeT, RequireCompleteness, SimpleFunction)
// They might also be a paramObj, to be filled in later
// TODO(2022): once generics are introduced, fieldType might be unneccesary
//case class StructT(fieldType: NewMapSubtype, params: NewMapObject) extends NewMapType
// TODO: What about simpler product types (no identifiers) based on MapT(TypeT, Count, CommandOutput, BasicMap)

// Params is a map from the fields of the struct to the Types (all the same level)
case class StructT(params: NewMapFunction) extends NewMapType

// cases: input type is the case constructors, output type is the field types per constructor
case class CaseT(cases: NewMapFunction) extends NewMapType

case class SubstitutableT(s: String, nType: NewMapSubtype) extends NewMapType

// Represents a type that contains a subset of the parent type, represented by a simple function
// - The output type of the simple function is usually a boolean (2) or at least a command type
// - Anything that's left at the initial value is NOT in the subtype
// For example, if the simple function is a Map from 10 to 2, and it reads (2: 1, 3: 1, 5: 1, 7: 1),
//  then the values 2, 3, 5, and 7 are considered part of this new type; the rest are not
case class SubtypeT(
  isMember: NewMapFunction
) extends NewMapSubtype

// Figure this out!
/*case class GenericType(
  parameters: Vector[(String, TypeParameter)],
  body: NewMapType
) extends NewMapType

case class TypeParameter(
  name: String,
  upperBounds: Vector[NewMapType] = Vector.empty,
  lowerBounds: Vector[NewMapType] = Vector.empty,
  variance: Option[TypeParameterVariance] = None, // If none - then variance is inferred
  typeDepth: Int = 0 
)

case class TypeParameterVariance(
  isCovariant: Boolean,
  isContravariant: Boolean
)*/

object NewMapO {
  
  def rangeT(i: Long): NewMapSubtype = SubtypeT(RangeFunc(i))

  // This is a subtype of TypeT, basically a newmap object with a command structure
  // - It has an initial value
  // - It has a command type
  // - You can give it commands to change the value
  // - You can potentially have versions available.
  def commandT(i: Long): NewMapSubtype = SubtypeT(IsCommandFunc(i))
}