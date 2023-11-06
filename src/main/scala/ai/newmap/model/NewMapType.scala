package ai.newmap.model

import ai.newmap.util.{Outcome, Success, Failure}

sealed abstract class NewMapType {
  override def toString = PrintNewMapObject.newMapType(this, Environment.Base.typeSystem)
  def displayString(env: Environment) = PrintNewMapObject.newMapType(this, env.typeSystem)

  def asUntagged: UntaggedObject = NewMapType.typeToUntaggedObject(this)

  def inputTypeOpt(uObjectOpt: Option[UntaggedObject]): Option[NewMapType] = NewMapType.getInputType(this, uObjectOpt)

  def upgradeCustom(custom: String, newVersion: Long): NewMapType = NewMapType.upgradeCustomType(this, custom, newVersion)
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

case class WildcardT(s: String) extends NewMapType
case class ParamIdT(s: String) extends NewMapType

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
  def toUntaggedObject: UntaggedObject = UArray()
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

object PartialMap extends MapCompleteness {
  override def getName: String = "PartialMap"
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
  //typeTransformPattern: USingularMap,

  // set of the actual types in the class
  typeSet: UntaggedObject,
) extends NewMapType

// cases: input type is the case constructors, output type is the field types per constructor
// TODO - this is not really a type because of the existance of type parameters. This is more of a type constructor
case class CaseT(
  cases: UntaggedObject,
  fieldParentType: NewMapType,
  featureSet: MapFeatureSet = BasicMap,
) extends NewMapType

//This is currently a shortcut for the following, which is a case that uses a number (the length) to understand the sequence type
/* CaseT(
      USingularMap(UWildcard("i"), MapT(
        TypeTransform(IndexT(ParamId("i")), valueType),
        config
      ).asUntagged),
      CountT,
      SimpleFunction,
    )
*/
case class SequenceT(
  parentType: NewMapType,
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
 * The values are cases, where the first item in the case is the length of the array, and the second is the case itself.
 * This should probably be implemented as Custom types (CustomT).
 * However, to start, it's probably easier to have them as explicit in the type system
 */
case class ArrayT(nType: NewMapType) extends NewMapType

case class CustomT(
  name: String,
  params: UntaggedObject,
  typeSystemId: Long // TODO - expand this with an ID!
) extends NewMapType

object NewMapType {
  def typeToUntaggedObject(nType: NewMapType): UntaggedObject = nType match {
    case CountT => UCase(UIdentifier("Count"), UArray())
    case IndexT(i) => UCase(UIdentifier("Index"), i)
    case BooleanT => UCase(UIdentifier("Boolean"), UArray())
    case ByteT => UCase(UIdentifier("Byte"), UArray())
    case CharacterT => UCase(UIdentifier("Character"), UArray())
    case LongT => UCase(UIdentifier("Long"), UArray())
    case DoubleT => UCase(UIdentifier("Double"), UArray())
    case UuidT => UCase(UIdentifier("Uuid"), UArray())
    case UndefinedT => UCase(UIdentifier("UndefinedType"), UArray())
    case TypeT => UCase(UIdentifier("Type"), UArray())
    case IdentifierT => UCase(UIdentifier("Identifier"), UArray())
    case MapT(TypeTransform(key, value), config) => UCase(UIdentifier("Map"), UArray(Array(
      USingularMap(typeToUntaggedObject(key), typeToUntaggedObject(value)),
      mapConfigToUntagged(config)
    )))
    case TypeTransformT(allowGenerics) => UCase(UIdentifier("TypeTransform"), UIndex(if (allowGenerics) 1 else 0))
    case StructT(params, fieldParentType, completenesss, featureSet) => UCase(UIdentifier("Struct"), UArray(Array(
      params,
      typeToUntaggedObject(fieldParentType),
      UIdentifier(completenesss.getName),
      UIdentifier(featureSet.getName)
    )))
    case TypeClassT(typeSet) => UCase(UIdentifier("TypeClass"), typeSet)
    case CaseT(cases, fieldParentType, featureSet) => UCase(UIdentifier("Case"), UArray(Array(
      cases,
      typeToUntaggedObject(fieldParentType),
      UIdentifier(featureSet.getName)
    )))
    case SubtypeT(isMember, parentType, featureSet) => UCase(UIdentifier("Subtype"), UArray(Array(
      isMember,
      typeToUntaggedObject(parentType),
      UIdentifier(featureSet.getName)
    )))
    case SequenceT(parentT, featureSet) => UCase(UIdentifier("Sequence"), UArray(parentT.asUntagged, UIdentifier(featureSet.getName)))
    case ArrayT(nType) => UCase(UIdentifier("Array"), nType.asUntagged)
    case CustomT(name, params, typeSystemId) => UCase(UIdentifier(name), UArray(params, UIndex(typeSystemId)))
    case WildcardT(name) => UWildcard(name)
    case ParamIdT(name) => ParamId(name)
  }

  def mapConfigToUntagged(config: MapConfig): UntaggedObject = {
    UArray(Array(
      UIdentifier(config.completeness.getName),
      UIdentifier(config.featureSet.getName),
      UArray(config.preservationRules.map(_.toUntaggedObject).toArray), // This should be a map in the future, not a struct
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
      case "Identifier" => Success(IdentifierT)
      case "Map" => params match {
        case UArray(items) if (items.length == 2) => {
          for {
            typeTransform <- convertToTypeTransform(items(0))

            config <- items(1) match {
              case UArray(v) if (v.length == 5) => Success(v)
              case _ => Failure(s"Incorrect config: ${items(1)}")
            }

            completeness <- config(0) match {
              case UIdentifier("RequireCompleteness") => Success(RequireCompleteness)
              case UIdentifier("CommandOutput") => Success(CommandOutput)
              case UIdentifier("PartialMap") => Success(PartialMap)
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
        case UArray(items) if (items.length == 4) => {
          for {
            parentT <- convertToNewMapType(items(1))

            completeness <- items(2) match {
              case UIdentifier("RequireCompleteness") => Success(RequireCompleteness)
              case UIdentifier("CommandOutput") => Success(CommandOutput)
              case UIdentifier("PartialMap") => Success(PartialMap)
              case _ => Failure(s"Can't allow feature set in struct type: ${items(2)}")
            }

            featureSet <- convertFeatureSet(items(3))
          } yield {
            StructT(items(0), parentT, completeness, featureSet)
          }
        }
        case _ => {
          throw new Exception(s"Couldn't convert Struct to NewMapType with params: $params")
          Failure(s"Couldn't convert Struct to NewMapType with params: $params")
        }
      }
      case "TypeClass" => Success(TypeClassT(params))
      case "Case" => params match {
        case UArray(items) if items.length == 3 => {
          for {
            parentT <- convertToNewMapType(items(1))

            featureSet <- convertFeatureSet(items(2))
          } yield CaseT(items(0), parentT, featureSet)
        }
        case _ => Failure(s"Couldn't convert Case to NewMapType with params: $params")
      }
      case "Sequence" => params match {
        case UArray(items) if items.length == 2 => {
          for {
            parentT <- convertToNewMapType(items(0))
            featureSet <- convertFeatureSet(items(1))
          } yield SequenceT(parentT, featureSet)
        }
        case _ => Failure(s"Couldn't convert Sequence to NewMapType with params: $params")
      }
      case "Subtype" => params match {
        case UArray(items) if items.length == 3 => {
          for {
            parentT <- convertToNewMapType(items(1))

            featureSet <- convertFeatureSet(items(2))

            // TODO: do we need this failWhen?
            _ <- Outcome.failWhen(
              featureSet.getLevel > SimpleFunction.getLevel,
              s"Can't allow feature set in subtype: ${items(2)}"
            )
          } yield SubtypeT(items(0), parentT, featureSet)
        }
        case _ => Failure(s"Couldn't convert Subtype to NewMapType with params: $params")
      }
      case "TypeTransform" => params match {
        case UIndex(i) if (i > 0) => {
          Success(TypeTransformT(true))
        }
        case _ => Success(TypeTransformT(false))
      }
      case "Array" => {
        for {
          nType <- convertToNewMapType(params)
        } yield ArrayT(nType)
      }
      case custom => params match {
        case UArray(items) if items.length == 2 => {
          val customParams = items(0)
          for {
            typeSystemId <- items(1) match {
              case UIndex(id) => Success(id)
              case _ => Failure("Unknown type system id: " + items(1))
            }
          } yield CustomT(custom, customParams, typeSystemId)
        }
        case _ => {
          Failure(s"Couldn't handle custom params for $custom: + $params")
        }
      }
    }
    case UInit => Success(UndefinedT)
    case UIndex(i) => Success(IndexT(UIndex(i)))
    case UIdentifier(name) => convertToNewMapType(UCase(UIdentifier(name), UInit))
    case UWildcard(name) => Success(WildcardT(name))
    case ParamId(name) => Success(ParamIdT(name))
    case _ => {
      throw new Exception(s"Couldn't convert to NewMapType: $uType")
      //Failure(s"Couldn't convert to NewMapType: $uType")
    }
  }

  def convertToTypeTransform(uObject: UntaggedObject): Outcome[TypeTransform, String] = {
    uObject match {
      case USingularMap(key, value) => {
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

  // Return the input type of a function type
  def getInputType(
    underlyingT: NewMapType,
    uObjectOpt: Option[UntaggedObject] // In some cases, this provides a shortcut to us but in general rely on underlyingT
  ): Option[NewMapType] = {
    underlyingT match {
      case StructT(_, parentFieldType, _, _) => Some(parentFieldType)
      case MapT(typeTransform, MapConfig(PartialMap, featureSet, _, _, _)) => {
        // If uObject is a parameter or something, this becomes impossible.
        // - we need to deal with this a little better, but we're close!
        // - ULTIMATELY thing of partial maps as a case type where the label is the keys that are available and the value is the map.
        //   - but we've shortcutted because the map itself contains all the relevant information
        uObjectOpt match {
          case Some(uObject) => Some(SubtypeT(uObject, typeTransform.keyType, featureSet))
          case None => None
        }
      }
      case MapT(typeTransform, _) =>  Some(typeTransform.keyType)
      case SequenceT(_, _) => {
        uObjectOpt match {
          case Some(UCase(i, _)) => Some(IndexT(i))
          case _ => None
        }
      }
      case _ => None
    }
  }

  def upgradeCustomType(nType: NewMapType, custom: String, newVersion: Long): NewMapType = {
    nType match {
      case MapT(TypeTransform(keyT, valueT), config) => {
        // TODO - there are going to be problems with upgrading the key, but we need to do it for now
        // - solution: check field maps and make sure they are all upgraded!
        val newKeyT = keyT.upgradeCustom(custom, newVersion)

        MapT(TypeTransform(newKeyT, valueT.upgradeCustom(custom, newVersion)), config)
      }
      case StructT(params, fieldParentType, completenesss, featureSet) => {
        val newParams = for {
          mapBinding <- params.getMapBindings.toOption.getOrElse(Vector.empty)
        } yield {
          val newParam = mapBinding._2.asType match {
            case Success(paramT) => paramT.upgradeCustom(custom, newVersion).asUntagged
            case _ => mapBinding._2
          }
          mapBinding._1 -> newParam
        }

        StructT(UMap(newParams), fieldParentType.upgradeCustom(custom, newVersion), completenesss, featureSet)
      }
      case TypeClassT(_) => {
        // TODO - we're going to have to redo these when we redo type classes
        nType
      }
      case CaseT(cases, fieldParentType, featureSet) => {
        val newCases = for {
          mapBinding <- cases.getMapBindings.toOption.getOrElse(Vector.empty)
        } yield {
          val newParam = mapBinding._2.asType match {
            case Success(paramT) => paramT.upgradeCustom(custom, newVersion).asUntagged
            case _ => mapBinding._2
          }

          mapBinding._1 -> newParam
        }

        CaseT(UMap(newCases), fieldParentType.upgradeCustom(custom, newVersion), featureSet)
      }
      case SubtypeT(isMember, parentType, featureSet) => {
        SubtypeT(isMember, parentType.upgradeCustom(custom, newVersion), featureSet) 
      }
      case CustomT(name, params, version) if (name == custom && version < newVersion) => {
        CustomT(name, params, newVersion)
      }
      case _ => nType
    }
  }
}
