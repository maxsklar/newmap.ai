package ai.newmap.model

import scala.collection.mutable.StringBuilder
import ai.newmap.model._
import java.util.UUID

object PrintNewMapObject {
  def apply(obj: NewMapObject): String = obj match {
    case CountT => "Count"
    case TypeT => s"Type"
    case AnyT => s"Any"
    case IsCommandFunc => s"IsCommandFunc"
    case IsSimpleFunction => s"IsSimpleFunction"
    case IsVersionedFunc => s"IsVersionedFunc"
    case IsConstantFunc => s"IsConstantFunc"
    case IsSubtypeFunc => s"IsSubtype"
    case IncrementFunc => s"Increment"
    case IdentifierT => "Identifier"
    case OrBooleanT => "OrBooleanT"
    case TaggedObject(uObject, nType) => untagged(uObject) + "\\" + this(nType)
    case MapT(key, value, completeness, featureSet) => {
      printMapT(this(key), this(value), completeness, featureSet)
    }
    case TableT(expandingKeyType, requiredValues) => {
      val mapTString = printMapT(this(expandingKeyType), this(requiredValues), RequireCompleteness, SimpleFunction)
      s"Table(${mapTString})"
    }
    case StructT(params) => "Struct " + this(params)
    case CaseT(cases) => "Case " + this(cases)
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
    case AccessField(struct, field) => s"${printExpression(struct)}.${this(field)}"
    case ParamId(name) => s"$name~pi"
    case BuildCase(constructor, input, caseType) => {
      this(caseType) + "." + this(constructor) + " " + printExpression(input)
    }
    case BuildMapT(inputType, outputType, completeness, featureSet) => {
      printMapT(printExpression(inputType), printExpression(outputType), completeness, featureSet)
    }
    case BuildTableT(expandingKeyType, requiredValues) => {
      val mapTString = printMapT(printExpression(expandingKeyType), printExpression(requiredValues), RequireCompleteness, SimpleFunction)
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
    case BuildMapInstance(values, mapT) => {
      mapToString(values)
    }
    case BuildStructInstance(values, structT) => {
      val sb: StringBuilder = new StringBuilder()
      sb.append("StructInstance(")
      var bindings: Vector[String] = Vector.empty
      for {
        (k, v) <- values
      } {
        bindings :+= k + ": " + printExpression(v)
      }
      sb.append(bindings.mkString(", "))
      sb.append(")")
      sb.toString
    }
    case BuildTableInstance(values, tableT) => {
      mapToString(values)
    }
  }

  def printMapT(
    key: String,
    value: String,
    completeness: MapCompleteness,
    featureSet: MapFeatureSet
  ): String = {
    (completeness, featureSet) match {
      case (CommandOutput, BasicMap) => "Map(" + key + ", " + value + ")"
      case (RequireCompleteness, SimpleFunction) => "ReqMap(" + key + ", " + value + ")"
      case (RequireCompleteness, FullFunction) => {
        // TODO(2022): Change the way lambda input works so that it's more like Map
        "\\(" + key + ": " + value + ")"
      }
      case _ => {
        // TODO(2022): Improve Notation so that we don't need this!
        "SpecialMap(" + key + ", " + value + ", " + completeness + ", " + featureSet + ")"
      }  
    }
  }

  def untagged(uObject: UntaggedObject): String = uObject match {
    case UIdentifier(s) => s
    case UMap(values) => mapToString(values)
    case UCase(constructor, value) => "(" + untagged(constructor) + " " + untagged(value) + ")"
    case UIndex(i) => i.toString
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
    case TypePattern(name, nType) => s"($name: ${this(nType)})"
    case StructPattern(params) => s"(${params.map(patternToString(_)).mkString(", ")})"
    case CasePattern(constructor, input) => s"(${untagged(constructor)} ${patternToString(input)})"
  }
}