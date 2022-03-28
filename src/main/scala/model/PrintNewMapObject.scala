package ai.newmap.model

import scala.collection.mutable.StringBuilder
import ai.newmap.model._
import java.util.UUID

object PrintNewMapObject {
  def apply(obj: NewMapObject): String = obj match {
    case Index(i) => i.toString
    case IndexValue(i, n) => i.toString + "\\" + n.toString
    case CountT => "Count"
    case TypeT => s"Type"
    case AnyT => s"Any"
    case IsCommandFunc => s"IsCommandFunc"
    case IsSimpleFunction => s"IsSimpleFunction"
    case IsVersionedFunc => s"IsVersionedFunc"
    case IncrementFunc => s"Increment"
    case IdentifierT => "Identifier"
    case IdentifierInstance(s) => s
    case MapT(key, value, completeness, featureSet) => {
      (completeness, featureSet) match {
        case (CommandOutput, BasicMap) => "Map(" + this(key) + ", " + this(value) + ")"
        case (RequireCompleteness, SimpleFunction) => "ReqMap(" + this(key) + ", " + this(value) + ")"
        case (RequireCompleteness, FullFunction) => {
          // TODO(2022): Change the way lambda input works so that it's more like Map
          "\\(" + this(key) + ": " + this(value) + ")"
        }
        case _ => {
          // TODO(2022): Improve Notation so that we don't need this!
          "SpecialMap(" + this(key) + ", " + this(value) + ", " + completeness + ", " + featureSet + ")"
        }  
      }
    }
    case MapInstance(values, mapT) => mapToString(values)
    case SequenceT(nType) => s"Sequence(${this(nType)})"
    case SequenceInstance(values, _) => "[" + values.map(this(_)).mkString(", ") + "]"
    case ApplyFunction(func, input) => {
      this(func) + " " + this(input)
    }
    case AccessField(struct, field) => s"${this(struct)}.${this(field)}"
    case ParamId(name) => s"$name~pi"
    case ParameterObj(uuid, nType) => s"$uuid~Po:(${this(nType)})"
    case StructT(params) => "Struct " + this(params)
    case CaseT(cases) => "Case " + this(cases)
    case StructInstance(value, structT) => {
      val sb: StringBuilder = new StringBuilder()
      sb.append("StructInstance(")
      var bindings: Vector[String] = Vector.empty
      for {
        (k, v) <- value
      } {
        bindings :+= k + ": " + this(v)
      }
      sb.append(bindings.mkString(", "))
      sb.append(")")
      sb.toString
    }
    case CaseInstance(constructor, value, _) => {
      constructor + " " + this(value) // Probably include the case here as well.
    }
    //TODO(2022): we might not want to print out the full parent here, because it could be large
    // - instead, we link to the function or map somehow... when we give things uniqueids we can figure this out
    case x@SubtypeT(isMember) => s"Subtype(${this(isMember)})"
    case VersionedObjectLink(uuid, status) => {
      // latestVersion(uuid: UUID, env: Environment): Outcome[Long, String]
      // currentState(uuid: UUID, env: Environment): Outcome[NewMapObject, String]
      s"VER[${uuid.toString}]"

      //this(currentState) + s"v$v"
    }
  }

  def printParams(params: Vector[(String, NewMapObject)]): String = {
    mapToString(params.map(x => ObjectPattern(IdentifierInstance(x._1)) -> x._2))
  }

  def mapToString(values: Vector[(NewMapPattern, NewMapObject)]): String = {
    val sb: StringBuilder = new StringBuilder()
    sb.append("(")

    var bindings: Vector[String] = Vector.empty
    for {
      (k, v) <- values
    } {
      bindings :+= patternToString(k) + ": " + this(v)
    }
    sb.append(bindings.mkString(", "))

    sb.append(")")
    sb.toString
  }

  def patternToString(nPattern: NewMapPattern): String = nPattern match {
    case ObjectPattern(nObject) => this(nObject)
    case TypePattern(name, nType) => s"$name: ${this(nType)}"
    case StructPattern(params) => s"(${params.map(patternToString(_)).mkString(", ")})"
  }
}