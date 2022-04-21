package ai.newmap.model

import scala.collection.mutable.StringBuilder
import ai.newmap.model._
import java.util.UUID

object PrintNewMapObject {
  def apply(obj: NewMapObject): String = obj match {
    case CountT => "Count"
    case TypeT => s"Type"
    case AnyT => s"Any"
    case IdentifierT => "Identifier"
    case OrBooleanT => "OrBooleanT"
    case TaggedObject(uObject, nType) => untagged(uObject) + "\\" + this(nType)
    case MapT(key, value, config) => {
      printMapT(this(key), this(value), config)
    }
    case ExpandingSubsetT(parentType, allowPattern) => {
      val p = if (allowPattern) "P" else ""
      s"ExpandingSubsetT$p(${this(parentType)})"
    }
    case StructT(params) => s"Struct(${this(params)})" 
    case CaseT(cases) => s"Case(${this(cases)})"
    //TODO(2022): we might not want to print out the full parent here, because it could be large
    // - instead, we link to the function or map somehow... when we give things uniqueids we can figure this out
    case x@SubtypeT(isMember) => s"Subtype(${this(isMember)})"
    case VersionedObjectLink(key, status) => {
      // latestVersion(uuid: UUID, env: Environment): Outcome[Long, String]
      // currentState(uuid: UUID, env: Environment): Outcome[NewMapObject, String]
      s"VER[${key.toString}]"

      //this(currentState) + s"v$v"
    }
  }

  def printExpression(
    nExpression: NewMapExpression
  ): String = nExpression match {
    case ObjectExpression(nObject) => this(nObject)
    case ApplyFunction(func, input) => {
      printExpression(func) + " " + printExpression(input)
    }
    case ParamId(name) => s"$name~pi"
    case BuildCase(constructor, input, caseType) => {
      this(caseType) + "." + this(constructor) + " " + printExpression(input)
    }
    case BuildMapT(inputType, outputType, config) => {
      printMapT(printExpression(inputType), printExpression(outputType), config)
    }
    case BuildTableT(expandingKeyType, requiredValues) => {
      val mapTString = printMapT(printExpression(expandingKeyType), printExpression(requiredValues), MapConfig(RequireCompleteness, SimpleFunction))
      s"Table(${mapTString})"
    }
    case BuildCaseT(cases) => {
      "Case " + printExpression(cases)
    }
    case BuildStructT(params) => {
      "Struct " + printExpression(params)
    }
    case BuildSubtypeT(isMember) => {
      s"Subtype(${printExpression(isMember)})"
    }
    case BuildMapInstance(values, nType) => {
      mapToString(values)
    }
    case BuildExpandingSubsetT(parentType, allowPattern) => {
      val p = if (allowPattern) "P" else ""

      s"ExpandingSubset$p(${printExpression(parentType)})"
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
          case SubtypeInput => "SubtypeInput"
        }

        val featureSetStr = config.featureSet match {
          case BasicMap => "BasicMap"
          case SimpleFunction => "Simple"
          case WellFoundedFunction => "WellFounded"
          case FullFunction => "FullFunction"
        }

        val modeCall = config.mode match {
          //case StructMode => "Struct"
          case _ => "Map"
        }

        // TODO(2022): Improve Notation so that we don't need this!
        s"$modeCall($key, $value, $completenessStr, $featureSetStr)"
      }  
    }
  }

  def untagged(uObject: UntaggedObject): String = uObject match {
    case UIdentifier(s) => s
    case UMap(values) => mapToString(values)
    case UCase(constructor, value) => "(" + untagged(constructor) + " " + untagged(value) + ")"
    case UType(nType) => this(nType)
    case UIndex(i) => i.toString
    case IsCommandFunc => s"IsCommandFunc"
    case IsSimpleFunction => s"IsSimpleFunction"
    case IncrementFunc => s"Increment"
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
    case CasePattern(constructor, input) => s"(${untagged(constructor)} ${patternToString(input)})"
  }
}