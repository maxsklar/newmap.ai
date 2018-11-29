package ai.newmap.model

/*
 * The types in the NewMap Language
 * This is actually a subset of the Objects, and there are functions below that convert between the 2
 */
sealed abstract class NewMapType{
  override def toString = PrintNewMapObject.applyType(this)
}

case class IndexT(i: Long) extends NewMapType
case object TypeT extends NewMapType
case object ObjectT extends NewMapType
case object IdentifierT extends NewMapType
case class MapT(key: NewMapType, value: NewMapType, default: NewMapObject) extends NewMapType
case class ParameterList(params: Vector[(String, NewMapType)]) extends NewMapType
case class LambdaT(params: Vector[(String, NewMapType)], result: NewMapType) extends NewMapType
case class SubstitutableT(s: String) extends NewMapType

// Represents a type that contains a subset of the parent type
case class Subtype(parent: NewMapType) extends NewMapType

// TODO: subtypes
// TODO: enumerated types
// TODO: other kinds of maps
// TODO: Callable Type?? (includes lambda and map)
