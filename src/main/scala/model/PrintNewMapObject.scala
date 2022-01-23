package ai.newmap.model

import scala.collection.mutable.StringBuilder
import ai.newmap.model._

object PrintNewMapObject {
  def apply(obj: NewMapObject): String = obj match {
    case Ord(i, false) => i.toString
    case Ord(0, true) => "Count"
    case Ord(i, true) => s"Count+$i"
    case TypeT => "Type"
    case CommandTypeT => "CommandType"
    case IdentifierT => "Identifier"
    case IdentifierInstance(s) => s + "~Id"
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
    case MapInstance(values) => "MI:" + mapToString(values)
    case LambdaInstance(lambdaParams, expression) => {
      val sb: StringBuilder = new StringBuilder()
      sb.append("(")
      var bindings: Vector[String] = Vector.empty

      lambdaParams match {
        case StructParams(params) => {
          for {
            (k, v) <- params
          } {
            bindings :+= k + ": " + this(v)
          }
        }
        case IdentifierParam(name, typeAsObj) => {
          bindings :+= name + ": " + this(typeAsObj)
        }
        case InputStackParam(typeAsObj) => {
          bindings :+= "_ : " + this(typeAsObj)
        }
      }

      sb.append(bindings.mkString(", "))
      sb.append(") => ")
      sb.append(this(expression))
      sb.toString
    }
    case ApplyFunction(func, input) => {
      this(func) + " " + this(input)
    }
    case ParameterObj(name) => name + "~Po"
    case StructT(fieldType, params) => "Struct " + this(fieldType) + " " + this(params)
    case CaseT(casesType, caseToType) => "Case " + this(casesType) + " " + this(caseToType)
    case StructInstance(value) => {
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
    case CaseInstance(constructor, value) => {
      constructor + " " + this(value)
    }
    //TODO(2022): we might not want to print out the full parent here, because it could be large
    // - instead, we link to the function or map somehow... when we give things uniqueids we can figure this out
    case Subtype(parent, func) => "Subtype(" + this(parent) + ", " + this(func) + ")"
    case SubstitutableT(s) => s  + "~St"
  }

  def printParams(params: Vector[(String, NewMapObject)]): String = {
    mapToString(params.map(x => IdentifierInstance(x._1) -> x._2))
  }

  def applyObjectWithType(nObjectWithType: NewMapObjectWithType): String = {
    apply(nObjectWithType.nObject) + " : " + (nObjectWithType.nTypeInfo match {
      case ExplicitlyTyped(nType) => apply(nType)
      case ImplicitlyTyped(convs) => "{" + convs.map(apply).mkString(", ") + "}"
    })
  }

  def mapToString(values: Vector[(NewMapObject, NewMapObject)]): String = {
    val sb: StringBuilder = new StringBuilder()
    sb.append("(")

    var bindings: Vector[String] = Vector.empty
    for {
      (k, v) <- values
    } {
      bindings :+= this(k) + ": " + this(v)
    }
    sb.append(bindings.mkString(", "))

    sb.append(")")
    sb.toString
  }
}