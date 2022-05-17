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

  def newMapType(nType: NewMapType): String = nType match {
    case CountT => "Count"
    case IndexT(i) => i.toString
    case CustomT(uuid, nType) => s"Type:$uuid:${newMapType(nType)}"
    case TypeT => s"Type"
    case UndefinedT => s"UndefinedType"
    //case AnyT => s"Any"
    case IdentifierT => "Identifier"
    case BooleanT => "BooleanT"
    case MapT(key, value, config) => {
      printMapT(newMapType(key), newMapType(value), config)
    }
    case GenericMapT(typeTransform, config) => {
      s"Generic(${mapToString(typeTransform)})"
    }
    case StructT(params, _, _, _) => s"Struct(${mapToString(params)})"
    case TypeClassT(typeTransform, implementation) => {
      s"TypeClassT(${mapToString(typeTransform)}, ${mapToString(implementation.map(x => (x -> ObjectExpression(UIndex(0)))))})"
    }
    case CaseT(cases, _, _) => {
      s"Case${mapToString(cases)}"
    }
    case ConstructedType(genericType, params) => {
      s"${this(genericType)}.${untagged(params)}"
    }
    //TODO(2022): we might not want to print out the full parent here, because it could be large
    // - instead, we link to the function or map somehow... when we give things uniqueids we can figure this out
    case SubtypeT(isMember, parentType, _) => s"Subtype(${untagged(isMember)})"
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
    case BuildMapT(inputType, outputType, config) => {
      printMapT(printExpression(inputType), printExpression(outputType), config)
    }
    case BuildGenericMapT(typeTransform, config) => {
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

  def untagged(uObject: UntaggedObject): String = uObject match {
    case UIdentifier(s) => s
    case UMap(values) => mapToString(values)
    case UCase(constructor, value) => "(" + untagged(constructor) + "." + untagged(value) + ")"
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
  }

  def mapToString(values: Vector[(NewMapPattern, NewMapExpression)]): String = {
    val sb: StringBuilder = new StringBuilder()
    sb.append("(")

    var bindings: Vector[String] = Vector.empty
    for {
      (k, v) <- values
    } {
      bindings :+= patternToString(k) + ": " + printExpression(v)
    }
    sb.append(bindings.mkString(", "))

    sb.append(")")
    sb.toString
  }

  def patternToString(nPattern: NewMapPattern): String = nPattern match {
    case ObjectPattern(nObject) => untagged(nObject)
    case WildcardPattern(name) => "W~" + name
    case StructPattern(params) => s"(${params.map(patternToString(_)).mkString(", ")})"
    case CasePattern(constructor, input) => s"(${untagged(constructor)}.${patternToString(input)})"
  }
}