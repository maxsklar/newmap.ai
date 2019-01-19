package ai.newmap.model

import scala.collection.mutable.StringBuilder
import ai.newmap.model._

object PrintNewMapObject {
  def apply(obj: NewMapObject): String = obj match {
    case Index(i) => i.toString
    case CountType => "Count"
    case TypeType => "Type"
    case IdentifierType => "Identifier"
    case IdentifierInstance(s) => s
    case MapType(key, value, default) => "(Map " + this(key) + " " + this(value) + " " + this(default) +")"
    case MapInstance(values, default) => {
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
    case LambdaType(typeTransformer) => {
      val sb: StringBuilder = new StringBuilder()
      sb.append("/\\")
      sb.append(this(typeTransformer))
      sb.toString
    }
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
    case ParameterObj(name) => name
    case StructType(params) => "Struct " + this(params)
    case CaseType(params) => "Case " + this(params)
    case StructInstance(value) => {
      val sb: StringBuilder = new StringBuilder()
      sb.append("(")
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
    case SubtypeType(parent) => "Subtype(" + this(parent) + ")"
    case SubtypeFromMap(mi) => mi match {
      case MapInstance(vals, default) => "Set" + vals.map(_._1).toString
    }
    case Increment => "increment"
    case AppendToSeq(currentSeq, newValue) => "appendSeq " + currentSeq + " " + newValue
    case AppendToMap(currentMap, newValues) => "appendMap " + currentMap + " " + newValues
    /*case MutableObject(commands, currentState) => {
      "V" + commands.length + "." + currentState.toString
    }
    case MutableType(staticType, init, commandType, updateFunction) => {
      "VersionedType (staticType: " + staticType + ", init: " + init + ", commandType: " + commandType + ", updateFunction " + updateFunction + ")"
    }*/
    case IncrementType(baseType) => {
      "IncrementType (" + baseType + ")"
    }
  }

  def applyType(nType: NewMapType): String = {
    this(ConvertNewMapTypeToObject(nType))
  }

  def applyObjectWithType(nObjectWithType: NewMapObjectWithType): String = {
    apply(nObjectWithType.nObject) + " : " + (nObjectWithType.nTypeInfo match {
      case ExplicitlyTyped(nType) => applyType(nType)
      case ImplicitlyTyped(convs) => "{" + convs.map(applyType).mkString(", ") + "}"
    })
  }
}