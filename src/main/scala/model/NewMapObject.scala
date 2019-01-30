package ai.newmap.model

/*
 * The objects in the NewMap Language
 * TODO - are these objects or expressions??
 */
sealed abstract class NewMapObject {
  override def toString = PrintNewMapObject(this)
}

case class Index(i: Long) extends NewMapObject

case object CountType extends NewMapObject

case object TypeType extends NewMapObject

case object IdentifierType extends NewMapObject

case class IdentifierInstance(s: String) extends NewMapObject

// TODO: now we're only supporting "map" but what about "UniqueMap", "IsoMap", "ReqMap", "Enum", etc.
case class MapType(key: NewMapObject, value: NewMapObject, default: NewMapObject) extends NewMapObject
case class MapInstance(
  values: Vector[(NewMapObject, NewMapObject)],
  default: NewMapObject
) extends NewMapObject

case class ReqMapType(key: NewMapObject, value: NewMapObject) extends NewMapObject
case class ReqMapInstance(values: Vector[(NewMapObject, NewMapObject)]) extends NewMapObject

case class LambdaType(
  typeTransformer: NewMapObject
) extends NewMapObject

// If Param == None, then 2 things happen:
// 1) If the input type is a struct, then the identifiers in that struct are used as variables
// 2) It gets pushed onto the input stack

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
case class StructType(
  params: NewMapObject // This must be of type MapType(Identifier, Type, 1)
) extends NewMapObject

case class CaseType(
  params: NewMapObject // This must be of type MapType(Identifier, Type, 0)
) extends NewMapObject

// This one is a little different/complex because each object has a unique type as defined by the struct
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

case class SubtypeType(parentType: NewMapObject) extends NewMapObject
case class SubtypeFromMap(map: ReqMapInstance) extends NewMapObject

// Basic Function Section
// These are pre-defined functions, their types are in comment

// Type Count => Count
case object Increment extends NewMapObject
case class IncrementType(baseType: NewMapObject) extends NewMapObject

//Type:
// (t: Type, currentSeq: Map n T default, t: T) => Map (increment n) T default
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

// converts a map into a type of it's keys
case object KeysOf extends NewMapObject

// This ultimately can be created when we have pattern matching and won't have to be here
// But for now it's defined by the pattern:
// Map K V v => Subtype K
case object KeysOfTypeTransformer extends NewMapObject
