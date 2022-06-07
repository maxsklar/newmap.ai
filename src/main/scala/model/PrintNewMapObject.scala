package ai.newmap.model

import scala.collection.mutable.StringBuilder
import ai.newmap.model._
import java.util.UUID

object PrintNewMapObject {
  def apply(obj: NewMapObject): String = obj match {
    case TaggedObject(uObject, nType) => untagged(uObject) + "\\" + newMapType(nType)
    case VersionedObjectLink(key) => {
      // latestVersion(uuid: UUID, env: Environment): Outcome[Long, String]
      // currentState(uuid: UUID, env: Environment): Outcome[NewMapObject, String]
      s"VER[${key.toString}]"

      //this(currentState) + s"v$v"
    }
  }

  // TODO - convert nType into an untagged type instead of using this!
  def newMapType(nType: NewMapType): String = nType match {
    case CountT => "Count"
    case IndexT(i) => "Index." + i.toString
    case CustomT(name, params) => {
      if (isEmptyMap(params)) {
        name
      } else {
        s"$name.${untagged(params)}"
      }
    }
    case TypeT => s"Type"
    case HistoricalTypeT(uuid) => s"HistoricalType($uuid)"
    case UndefinedT => s"UndefinedType"
    //case AnyT => s"Any"
    case IdentifierT => "Identifier"
    case BooleanT => "Boolean"
    case ByteT => "Byte"
    case CharacterT  => "Character"
    case StringT => "String"
    case LongT => "Long"
    case DoubleT => "Double"
    case UuidT => "UUID"
    case MapT(typeTransform, config) => {
      if (typeTransform.length == 1) {
        val inputType = typeTransform.head._1
        val outputTypeExp = typeTransform.head._2
        printMapT(untagged(inputType), printExpression(outputTypeExp), config)
      } else {
        s"Generic(${mapToString(typeTransform)})"
      }
    }
    //case StructT(params, parentType, completeness, featureSet) => s"Struct(${mapToString(params)})~$parentType~$completeness~$featureSet"
    case StructT(params, parentType, completeness, featureSet) => s"Struct(${mapToString(params)})"
    case TypeClassT(typeTransform, implementation) => {
      s"TypeClassT(${mapToString(typeTransform)}, ${mapToString(implementation.map(x => (x -> ObjectExpression(UIndex(0)))))})"
    }
    case CaseT(cases, _, _) => {
      s"Case${mapToString(cases)}"
    }
    /*case ConstructedType(genericType, params) => {
      s"${this(genericType)}.${untagged(params)}"
    }*/
    //TODO(2022): we might not want to print out the full parent here, because it could be large
    // - instead, we link to the function or map somehow... when we give things uniqueids we can figure this out
    case SubtypeT(isMember, parentType, _) => s"Subtype(${untagged(isMember)})"
    case WithStateT(uuid, nType) => s"WithState:$uuid:${newMapType(nType)}" 
    case WildcardPatternT(name) => untagged(UWildcardPattern(name))
  }

  def printExpression(
    nExpression: NewMapExpression
  ): String = nExpression match {
    case ObjectExpression(nObject) => untagged(nObject)
    case ApplyFunction(func, input) => {
      printExpression(func) + " " + printExpression(input)
    }
    case ParamId(name) => s"$name~pi"
    case BuildCase(constructor, input) => {
      untagged(constructor) + "." + printExpression(input)
    }
    case BuildSimpleMapT(inputType, outputType, config) => {
      printMapT(printExpression(inputType), printExpression(outputType), config)
    }
    case BuildMapT(typeTransform, config) => {
      s"Generic(${printExpression(typeTransform)})"
    }
    case BuildTableT(expandingKeyType, requiredValues) => {
      val mapTString = printMapT(printExpression(expandingKeyType), printExpression(requiredValues), MapConfig(RequireCompleteness, SimpleFunction))
      s"Table(${mapTString})"
    }
    case BuildCaseT(cases, _, _) => {
      "Case " + printExpression(cases)
    }
    case BuildStructT(params, _, _, _) => {
      "Struct " + printExpression(params)
    }
    case BuildSubtypeT(isMember, _, _) => {
      s"Subtype(${printExpression(isMember)})"
    }
    case BuildNewTypeClassT(typeTransform) => {
      s"new TypeClassT(${printExpression(typeTransform)})}"
    }
    case BuildMapInstance(values) => {
      mapToString(values)
    }
  }

  def printMapT(
    key: String,
    value: String,
    config: MapConfig
  ): String = {
    (config.completeness, config.featureSet) match {
      case (CommandOutput, BasicMap) => "Map(" + key + ", " + value + ")"
      case (RequireCompleteness, SimpleFunction) => "ReqMap(" + key + ", " + value + ")"
      case (RequireCompleteness, FullFunction) => {
        // TODO(2022): Change the way lambda input works so that it's more like Map
        "\\(" + key + ": " + value + ")"
      }
      case _ => {
        val completenessStr = config.completeness match {
          case RequireCompleteness => "Required"
          case CommandOutput => "Command"
        }

        val featureSetStr = config.featureSet match {
          case BasicMap => "BasicMap"
          case SimpleFunction => "Simple"
          case WellFoundedFunction => "WellFounded"
          case FullFunction => "FullFunction"
        }

        // TODO(2022): Improve Notation so that we don't need this!
        s"Map($key, $value, $completenessStr, $featureSetStr)"
      }  
    }
  }

  def isEmptyMap(value: UntaggedObject): Boolean = value match {
    case UStruct(v) => v.isEmpty
    case UMap(v) => v.isEmpty
    case _ => false
  }

  def untagged(uObject: UntaggedObject): String = uObject match {
    case UIdentifier(s) => s
    case UMap(values) => mapToString(values)
    case UStruct(values) => sequenceToString(values)
    case UCase(constructor, value) => {
      if (isEmptyMap(value)) {
        untagged(constructor)
      } else {
        untagged(constructor) + "." + untagged(value)
      }
    }
    case UType(nType) => newMapType(nType)
    case UIndex(i) => i.toString
    case UInit => "()"
    case IsCommandFunc => s"IsCommandFunc"
    case IncrementFunc => s"Increment"
    case ULink(key) => {
      s"ULink[${key.toString}]"
    }
    case UParametrizedCaseT(typeParameters, caseT) => {
      val typeParameterNames = typeParameters.map(_._1)
      val typeParametersStr = s"[${typeParameterNames.mkString(", ")}]"
      s"Case${typeParametersStr}${mapToString(caseT.cases)}"
    }
    case UByte(value: Byte) => s"$value"
    case UCharacter(value: Char) => s"$value"
    case UString(value: String) => s"$value~str"
    case ULong(value: Long) => s"$value"
    case UDouble(value: Double) => s"$value"
    case Uuuid(value) => s"$value"
    case UWildcardPattern(name) => "W~" + name
    case UParamId(name) => s"$name~pi"
    case UMapTPattern(input, output, config) => printMapT(untagged(input), untagged(output), config)
  }

  def mapToString(values: Vector[(UntaggedObject, NewMapExpression)]): String = {
    val sb: StringBuilder = new StringBuilder()
    sb.append("(")

    var bindings: Vector[String] = Vector.empty
    for {
      (k, v) <- values
    } {
      bindings :+= untagged(k) + ": " + printExpression(v)
    }
    sb.append(bindings.mkString(", "))

    sb.append(")")
    sb.toString
  }

  def sequenceToString(values: Vector[UntaggedObject]): String = {
    val sb: StringBuilder = new StringBuilder()
    sb.append("(")

    var bindings: Vector[String] = Vector.empty
    for {
      v <- values
    } {
      bindings :+= untagged(v)
    }
    sb.append(bindings.mkString(", "))

    sb.append(")")
    sb.toString
  }
}