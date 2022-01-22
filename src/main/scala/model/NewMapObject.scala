package ai.newmap.model

/*
 * The objects in the NewMap Language
 */
sealed abstract class NewMapObject {
  override def toString = PrintNewMapObject(this)
}

case class IdentifierInstance(s: String) extends NewMapObject

case class MapInstance(
  values: Vector[(NewMapObject, NewMapObject)]
) extends NewMapObject

// There are several different ways items can be passed into a lambda expression
sealed abstract class LambdaParamStrategy

// The parameter type is a struct, and the name of the parameters is how the values are called
case class StructParams(params: Vector[(String, NewMapObject)]) extends LambdaParamStrategy

// The parameter is named by an identifier
case class IdentifierParam(name: String, typeAsObj: NewMapObject) extends LambdaParamStrategy

// The parameter is pushed onto an input stack
case class InputStackParam(typeAsObj: NewMapObject) extends LambdaParamStrategy

case class LambdaInstance(
  paramStrategy: LambdaParamStrategy,
  expression: NewMapObject
) extends NewMapObject

case class ApplyFunction(
  func: NewMapObject,
  input: NewMapObject
) extends NewMapObject

// This is an object that we don't know the value of yet. It'll be passed in later.
case class ParameterObj(name: String) extends NewMapObject

// This is like a map instance, but it represents a struct (Map from Identifiers to Types)
/*case class StructType(
  params: NewMapObject // This must be of type MapType(Identifier, Type, 1)
) extends NewMapObject

case class CaseType(
  params: NewMapObject // This must be of type MapType(Identifier, Type, 0)
) extends NewMapObject*/

// This one is a little different/complex because each object has a unique type as defined by the struct
// TODO: should we merge this with MapInstance, since a type is going to be attached anyway!
case class StructInstance(value: Vector[(String, NewMapObject)]) extends NewMapObject

case class CaseInstance(constructor: String, input: NewMapObject) extends NewMapObject

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

//Type:
// (t: Type, currentSeq: Map n T default, t: T) => Map (increment n) T default
// TODO: Redo this with respect to commands
case class AppendToSeq(
  currentSeq: NewMapObject,
  newValue: NewMapObject
) extends NewMapObject

// Type:
// (keyType: Type, valueType: Type, currentMap: Map keyType valueType default, appendedMap: Map keyType valueType default) => Map keyType valueType default
case class AppendToMap(
  currentSeq: NewMapObject,
  newValues: NewMapObject
) extends NewMapObject


// Mutables Section

// Encapsulates all possible mutable objects (stacks, sequences, types, and counts)
/*case class MutableObject(
  commands: Vector[NewMapObject],
  currentState: NewMapObject
) extends NewMapObject

case class MutableType(
  staticType: NewMapObject,
  init: NewMapObject,
  commandType: NewMapObject,
  updateFunction: NewMapObject
) extends NewMapObject*/

/*
 * The types in the NewMap Language
 * This is actually a subset of the Objects, and there are functions below that convert between the 2
 *
 * TODO - this should all be moved to NewMapObject, so that way it can be sealed
 */
sealed abstract class NewMapType extends NewMapObject

case class Index(i: Long) extends NewMapType
case object CountT extends NewMapType

// Type of types.. very confusing!
case object TypeT extends NewMapType

// This is a subtype of TypeT, basically a newmap object with a command structure
// - It has an initial value
// - It has a command type
// - You can give it commands to change the value
// - You can potentially have versions available.
case object CommandTypeT extends NewMapType


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
  inputType: NewMapObject,
  outputType: NewMapObject,
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


// TODO: these should actually be connected to NewMapObjects which are of type
//  MapT(TypeT, Count, CommandOutput, BasicMap)
//case class StructT(params: Vector[(String, NewMapObject)]) extends NewMapType

// Params should be connected to a NewMapObject which are of type
//  MapT(fieldType, TypeT, RequireCompleteness, BasicMap)
// They might also be a paramObj, to be filled in later
// TODO(2022): once generics are introduced, fieldType might be unneccesary
case class StructT(fieldType: NewMapObject, params: NewMapObject) extends NewMapType
// TODO: What about simpler product types (no identifiers) based on MapT(TypeT, Count, CommandOutput, BasicMap)

case class CaseT(params: Vector[(String, NewMapObject)]) extends NewMapType

case class SubstitutableT(s: String) extends NewMapType

// Represents a type that contains a subset of the parent type, represented by a simple function
// - The output type of the simple function is usually a boolean (2) or at least a command type
// - Anything that's left at the initial value is NOT in the subtype
// For example, if the simple function is a Map from 10 to 2, and it reads (2: 1, 3: 1, 5: 1, 7: 1),
//  then the values 2, 3, 5, and 7 are considered part of this new type; the rest are not
//TODO(2022): Change once simpleFunction is required to be type checked already.
case class Subtype(
  parentType: NewMapObject,
  simpleFunction: NewMapObject
) extends NewMapType


// TODO: Type functions

// TODO: Examine Well-foundedness
// Concrete Types: IndexT, IdentifierT
//   MapT when the inputs are well founded
//   StructT when the inputs are well founded
//   LambdaT when the inputs are well founded
//   SubstitutableT when the inputs are well founded
// Type Classes
//   TypeT
//   ObjectT
//   Any of the type constructors when the inputs are not well founded.
// Plus with IndexT (and eventually count) it's types all the way down!

// IDEA: talk about types and sets (subtypes of the type)
//  types can be narrowed down by their sets
//  Type Classes will come in later!
// RAW TYPES:
//  -- Index/Count (increments) - each count is also an index
//  -- IdentifierT (just unique phrases)
//  -- maps (command = key + command for the value type)
//  -- lambda expression
// WHAT NEXT
// -- Then, any subset or enum can be used as a type
// IMPLICATIONS OF THIS
// -- Each variable has a type, a subset, and a value
// -- val x: TYPE = VALUE
//  TYPE can either be an actual type or a subset (if it's a subset, the actual type is inferred)
//  The VALUE cannot refer to an actual type, but can refer to a subset.
//  Later on, we can create VALUEs that model the type system, and generate newmap code.

// This also means that param maps are kind of a meta map - and they can be versioned as well - but in a separate domain

// What about type classes?

// Idea: Concrete Types should be specifiable as objects
//   Then type classes or abstract types??

// TODO: enumerated types
// TODO: other kinds of maps
// TODO: Callable Type?? (includes lambda and map)

