package ai.newmap.model

import scala.collection.mutable.StringBuilder
import ai.newmap.model._

object PrintNewMapObject {
  def apply(obj: NewMapObject): String = obj match {
    case Index(i) => i.toString
    case CountT => "Count"
    case TypeT => s"Type"
    case AnyT => s"Any"
    case IsCommandFunc => s"IsCommandFunc"
    case IdentifierT => "Identifier"
    case IdentifierInstance(s) => s + "~Id"
    case MapT(key, value, completeness, featureSet) => {
      (completeness, featureSet) match {
        case (CommandOutput, BasicMap) => "Map(" + this(key) + ", " + this(value) + ")-" + completeness + "-" + featureSet
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
    case MapInstance(values, MapT(_, _, _, featureSet)) => s"MI:${mapToString(values)}"
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
      }

      sb.append(bindings.mkString(", "))
      sb.append(") => ")
      sb.append(this(expression))
      sb.toString
    }
    case ApplyFunction(func, input) => {
      this(func) + " " + this(input)
    }
    case AccessField(struct, field) => s"${this(struct)}.${this(field)}"
    case ParameterObj(name, nType) => s"$name~Po:(${this(nType)})"
    case StructT(params) => "Struct " + this(params)
    case CaseT(cases) => "Case " + this(cases)
    case StructInstance(value, _) => {
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
      constructor + " " + this(value)
    }
    //TODO(2022): we might not want to print out the full parent here, because it could be large
    // - instead, we link to the function or map somehow... when we give things uniqueids we can figure this out
    case x@SubtypeT(isMember) => s"Subtype(${this(isMember)})"
    case SubstitutableT(s, y) => s"$s~St~$y"
    case RangeFunc(i) => s"<$i"
  }

  def printParams(params: Vector[(String, NewMapObject)]): String = {
    mapToString(params.map(x => IdentifierInstance(x._1) -> x._2))
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