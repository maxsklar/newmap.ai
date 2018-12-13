package ai.newmap.model

/*
 * The objects in the NewMap Language
 * TODO - are these objects or expressions??
 */
sealed abstract class NewMapObject {
  override def toString = PrintNewMapObject(this)
}

case class Index(i: Long) extends NewMapObject

case object TypeType extends NewMapObject

case object IdentifierType extends NewMapObject

case class IdentifierInstance(s: String) extends NewMapObject

// TODO: now we're only supporting "map" but what about "UniqueMap", "IsoMap", "ReqMap", "Enum", etc.
case class MapType(key: NewMapObject, value: NewMapObject, default: NewMapObject) extends NewMapObject
case class MapInstance(
  values: Vector[(NewMapObject, NewMapObject)],
  default: NewMapObject
) extends NewMapObject

// This could be a type if the expression is also a type
case class LambdaInstance(
  params: Vector[(String, NewMapObject)],
  expression: NewMapObject
) extends NewMapObject

case class ApplyFunction(
  func: NewMapObject,
  input: NewMapObject
) extends NewMapObject

// This is an object that we don't know the value of yet. It'll be passed in later.
case class ParameterObj(name: String) extends NewMapObject

// This is like a map instance, but it represents a struct (Map from Identifiers to Types)
case class StructType(
  params: NewMapObject // This must be of type MapType(Identifier, Type, 1)
) extends NewMapObject

// This one is a little different/complex because each object has a unique type as defined by the struct
case class StructInstance(value: Vector[(String, NewMapObject)]) extends NewMapObject

case class SubtypeType(parentType: NewMapObject) extends NewMapObject
case class SubtypeFromMap(map: MapInstance) extends NewMapObject