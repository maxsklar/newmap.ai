package ai.newmap.model

import java.util.UUID
import ai.newmap.util.{Outcome, Success, Failure}

sealed abstract class NewMapType {
  override def toString = PrintNewMapObject.newMapType(this, Environment.Base.typeSystem)
  def displayString(env: Environment) = PrintNewMapObject.newMapType(this, env.typeSystem)

  def asUntagged: UntaggedObject = NewMapType.typeToUntaggedObject(this)
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
 * A map is essentially a function or key-value store, and there are 5 levels:
 * - The first (BasicMap) is a map where each key-value pair needs to be specified directly.
 *   BasicMap has the distinction of always being finite!
 * - The third is a simple function, which can be coded but it's likely to be executed quickly
 *   (In other words, the coding set is not turing complete and infinite loops are avoided.
 *    we can thus execute simple functions without too much worry about compute time
 * - The fifth is the full function - which has a full coding set to turn an input into an output
 *
 * The completeness field also has several options:
 * - requireAllFields tells us that we are required to specify an output for
 *   all potential inputs. It's smart to turn this on to ensure that functions and maps are checked as complete
 * - commandOutput means that the output types must all be command types, which means they start at an initial value.
 */
case class MapT(
  typeTransform: TypeTransform,
  config: MapConfig,
) extends NewMapType

case class TypeTransform(
  keyType: NewMapType,
  valueType: NewMapType
)

// This is the type of a type transform
// It's not just a pair of types because the keyType is a pattern that feeds into the value type.
case class TypeTransformT(allowGenerics: Boolean) extends NewMapType

case class MapConfig(
  completeness: MapCompleteness,
  featureSet: MapFeatureSet,
  preservationRules: Vector[PreservationRule] = Vector.empty,
  channels: UntaggedObject = UMap(Vector.empty),
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
  params: UntaggedObject,
  fieldParentType: NewMapType,
  completeness: MapCompleteness = RequireCompleteness,
  featureSet: MapFeatureSet = BasicMap
) extends NewMapType

// Is this really a type? Or just an instance of SubtypeT(TypeT)?
case class TypeClassT(
  // The typeTransform encodes the abstract values that this type class is required to have.
  // Eg (t: t => String) means that every type in this class can calculate a String value for all it's objects.
  // Note that this value isn't named, but its'
  typeTransformPattern: UMapPattern,

  // List of the actual types in the class
  // Along with their implementation
  typesInTypeClass: UntaggedObject,
) extends NewMapType

// cases: input type is the case constructors, output type is the field types per constructor
// TODO - this is not really a type because of the existance of type parameters. This is more of a type constructor
case class CaseT(
  cases: UntaggedObject,
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

object NewMapType {
  def typeToUntaggedObject(nType: NewMapType): UntaggedObject = nType match {
    case CountT => UCase(UIdentifier("Count"), UStruct(Vector.empty))
    case IndexT(i) => UCase(UIdentifier("Index"), i)
    case BooleanT => UCase(UIdentifier("Boolean"), UStruct(Vector.empty))
    case ByteT => UCase(UIdentifier("Byte"), UStruct(Vector.empty))
    case CharacterT => UCase(UIdentifier("Character"), UStruct(Vector.empty))
    case LongT => UCase(UIdentifier("Long"), UStruct(Vector.empty))
    case DoubleT => UCase(UIdentifier("Double"), UStruct(Vector.empty))
    case UuidT => UCase(UIdentifier("Uuid"), UStruct(Vector.empty))
    case UndefinedT => UCase(UIdentifier("UndefinedType"), UStruct(Vector.empty))
    case TypeT => UCase(UIdentifier("Type"), UStruct(Vector.empty))
    case HistoricalTypeT(uuid) => UCase(UIdentifier("HistoricalType"), Uuuid(uuid))
    case IdentifierT => UCase(UIdentifier("Identifier"), UStruct(Vector.empty))
    case MapT(TypeTransform(key, value), config) => UCase(UIdentifier("Map"), UStruct(Vector(
      UMapPattern(typeToUntaggedObject(key), typeToUntaggedObject(value)),
      mapConfigToUntagged(config)
    )))
    case TypeTransformT(allowGenerics) => UCase(UIdentifier("TypeTransform"), UIndex(if (allowGenerics) 1 else 0))
    case StructT(params, fieldParentType, completenesss, featureSet) => UCase(UIdentifier("Struct"), UStruct(Vector(
      params,
      typeToUntaggedObject(fieldParentType),
      UIdentifier(completenesss.getName),
      UIdentifier(featureSet.getName)
    )))
    case TypeClassT(typeTransform, typesInTypeClass) => UCase(UIdentifier("TypeClass"), UStruct(Vector(
      typeTransform,
      typesInTypeClass
    )))
    case CaseT(cases, fieldParentType, featureSet) => UCase(UIdentifier("Case"), UStruct(Vector(
      cases,
      typeToUntaggedObject(fieldParentType),
      UIdentifier(featureSet.getName)
    )))
    case SubtypeT(isMember, parentType, featureSet) => UCase(UIdentifier("Subtype"), UStruct(Vector(
      isMember,
      typeToUntaggedObject(parentType),
      UIdentifier(featureSet.getName)
    )))
    case FunctionalSystemT(functionTypes) => UCase(UIdentifier("FunctionalSystem"), UMap(functionTypes))
    case CustomT(name, params) => UCase(UIdentifier(name), params)
    case WithStateT(uuid, nType) => {
      UCase(
        UIdentifier("WithState"),
        UCase(
          Uuuid(uuid),
          typeToUntaggedObject(nType)
        )
      )
    }
    case WildcardPatternT(name) => UWildcardPattern(name)
    case ParamIdT(name) => ParamId(name)
  }

  def mapConfigToUntagged(config: MapConfig): UntaggedObject = {
    UStruct(Vector(
      UIdentifier(config.completeness.getName),
      UIdentifier(config.featureSet.getName),
      UStruct(config.preservationRules.map(_.toUntaggedObject)), // This should be a map in the future, not a struct
      config.channels,
      typeToUntaggedObject(config.channelParentType)
    ))
  }

  def convertToNewMapType(
    uType: UntaggedObject
  ): Outcome[NewMapType, String] = uType match {
    case UCase(UIdentifier(identifier), params) => identifier match {
      case "Count" => Success(CountT)
      case "Index" => Success(IndexT(params))
      case "Boolean" => Success(BooleanT)
      case "Byte" => Success(ByteT)
      case "Character" => Success(CharacterT)
      //case "String" => Success(StringT)
      case "Long" => Success(LongT)
      case "Double" => Success(DoubleT)
      case "Uuid" => Success(UuidT)
      case "UndefinedType" => Success(UndefinedT)
      case "Type" => Success(TypeT)
      case "HistoricalType" => params match {
        case Uuuid(uuid) => Success(HistoricalTypeT(uuid))
        case _ => Failure(s"Couldn't convert HistoricalType to NewMapType with params: $params")
      }
      case "Identifier" => Success(IdentifierT)
      case "Map" => params match {
        case UStruct(items) if (items.length == 2) => {
          for {
            typeTransform <- convertToTypeTransform(items(0))

            config <- items(1) match {
              case UStruct(v) if (v.length == 5) => Success(v)
              case _ => Failure(s"Incorrect config: ${items(1)}")
            }

            completeness <- config(0) match {
              case UIdentifier("RequireCompleteness") => Success(RequireCompleteness)
              case UIdentifier("CommandOutput") => Success(CommandOutput)
              case _ => Failure(s"Can't allow feature set in struct type: ${items(1)}")
            }

            featureSet <- convertFeatureSet(config(1))

            // TODO: config(2) for the preservation rules
            // TODO: config(3) for channels
            // TODO: config(4) for parent of channel names

          } yield {
            MapT(typeTransform, MapConfig(completeness, featureSet))
          }
        }
        case _ => Failure(s"Couldn't convert Map to NewMapType with params: $params")
      }
      case "Struct" => params match {
        case UStruct(items) if (items.length == 4) => {
          for {
            parentT <- convertToNewMapType(items(1))

            completeness <- items(2) match {
              case UIdentifier("RequireCompleteness") => Success(RequireCompleteness)
              case UIdentifier("CommandOutput") => Success(CommandOutput)
              case _ => Failure(s"Can't allow feature set in struct type: ${items(2)}")
            }

            featureSet <- convertFeatureSet(items(3))
          } yield {
            StructT(items(0), parentT, completeness, featureSet)
          }
        }
        case _ => Failure(s"Couldn't convert Struct to NewMapType with params: $params")
      }
      case "TypeClass" => params match {
        case UStruct(items) if (items.length == 2) => {
          for {
            typeTransform <- items(0) match {
              case ump@UMapPattern(_, _) => Success(ump)
              case UMap(uMap) if (uMap.length <= 1) => uMap.headOption match {
                case Some(singleton) => Success(UMapPattern(singleton._1, singleton._2))
                case None => Failure("Empty Type Class Pattern")
              }
              case _ => Failure(s"Invalid typeTransform in TypeClass: ${items(0)}")
            }
          } yield {
            TypeClassT(typeTransform, items(1))
          }
        }
        case _ => Failure(s"Couldn't convert TypeClass to NewMapType with params: $params")
      }
      case "Case" => params match {
        case UStruct(items) if items.length == 3 => {
          for {
            parentT <- convertToNewMapType(items(1))

            featureSet <- convertFeatureSet(items(2))
          } yield CaseT(items(0), parentT, featureSet)
        }
        case _ => Failure(s"Couldn't convert Case to NewMapType with params: $params")
      }
      case "Subtype" => params match {
        case UStruct(items) if items.length == 3 => {
          for {
            parentT <- convertToNewMapType(items(1))

            featureSet <- convertFeatureSet(items(2))

            _ <- Outcome.failWhen(
              featureSet.getLevel > SimpleFunction.getLevel,
              s"Can't allow feature set in subtype: ${items(2)}"
            )
          } yield SubtypeT(items(0), parentT, featureSet)
        }
        case _ => Failure(s"Couldn't convert Subtype to NewMapType with params: $params")
      }
      case "WithState" => params match {
        case UCase(Uuuid(uuid), t) => {
          for {
            resultT <- convertToNewMapType(t)
          } yield {
            WithStateT(uuid, resultT)
          }
        }
        case _ => Failure(s"Couldn't convert WithState to NewMapType with params: $params")
      }
      case custom => Success(CustomT(custom, params))
    }
    case UInit => Success(UndefinedT)
    case UIndex(i) => Success(IndexT(UIndex(i)))
    case UIdentifier(name) => convertToNewMapType(UCase(UIdentifier(name), UInit))
    case UWildcardPattern(name) => Success(WildcardPatternT(name))
    case ParamId(name) => Success(ParamIdT(name))
    case _ => {
      throw new Exception(s"Couldn't convert to NewMapType: $uType")
      Failure(s"Couldn't convert to NewMapType: $uType")
    }
  }

  def convertToTypeTransform(uObject: UntaggedObject): Outcome[TypeTransform, String] = {
    uObject match {
      case UMapPattern(key, value) => {
        for {
          keyType <- convertToNewMapType(key)
          valueType <- convertToNewMapType(value)
        } yield TypeTransform(keyType, valueType)
      }
      case UMap(Vector(pair)) => {
        for {
          keyType <- convertToNewMapType(pair._1)
          valueType <- convertToNewMapType(pair._2)
        } yield TypeTransform(keyType, valueType)
      }
      case _ => Failure("Couldn't build type transform for " + uObject)
    }
  }

  def convertFeatureSet(uObject: UntaggedObject): Outcome[MapFeatureSet, String] = uObject match {
    case UIdentifier("BasicMap") => Success(BasicMap)
    case UIdentifier("PatternMap") => Success(PatternMap)
    case UIdentifier("SimpleFunction") => Success(SimpleFunction)
    case UIdentifier("WellFoundedFunction") => Success(WellFoundedFunction)
    case UIdentifier("FullFunction") => Success(FullFunction)
    case _ => Failure(s"Can't allow feature set in struct type: $uObject")
  }
}
