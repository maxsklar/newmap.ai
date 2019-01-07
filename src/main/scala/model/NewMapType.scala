package ai.newmap.model

/*
 * The types in the NewMap Language
 * This is actually a subset of the Objects, and there are functions below that convert between the 2
 */
sealed abstract class NewMapType{
  override def toString = PrintNewMapObject.applyType(this)
}

case class IndexT(i: Long) extends NewMapType

// This is meant to apply to an index, increment the type by 1.
// Can it apply to other types as well? Remains to be seen, but not checked as of yet.
// TODO - there's a lot that needs to be figured out with this one!
// Perhaps this doesn't need to be type checked because it's only used internally
case class IncrementT(base: NewMapType) extends NewMapType

case object TypeT extends NewMapType
case object CountT extends NewMapType
case object IdentifierT extends NewMapType
case class MapT(key: NewMapType, value: NewMapType, default: NewMapObject) extends NewMapType
case class StructT(params: Vector[(String, NewMapType)]) extends NewMapType
case class CaseT(params: Vector[(String, NewMapType)]) extends NewMapType


// Actually the result type can depend on the input type
case class LambdaT(input: NewMapType, result: NewMapType) extends NewMapType

case class SubstitutableT(s: String) extends NewMapType

// Represents a type that contains a subset of the parent type
case class Subtype(parent: NewMapType) extends NewMapType
case class SubtypeFromMapType(values: MapInstance) extends NewMapType

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


// TODO: subtypes need to be fleshed out more

// TODO: enumerated types
// TODO: other kinds of maps
// TODO: Callable Type?? (includes lambda and map)
