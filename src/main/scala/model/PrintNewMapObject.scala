package ai.newmap.model

import scala.collection.mutable.StringBuilder

object PrintNewMapObject {
  def apply(obj: NewMapObject): String = obj match {
    case Index(i) => i.toString
    case ObjectType => "Object"
    case TypeType => "Type"
    case IdentifierType => "Identifier"
    case IdentifierInstance(s) => s
    case MapType(key, value, default) => "Map (" + this(key) + ") => (" + this(value) + ") default " + this(default)
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
    case LambdaInstance(params, expression) => {
      val sb: StringBuilder = new StringBuilder()
      sb.append("(")
      var bindings: Vector[String] = Vector.empty
      for {
        (k, v) <- params
      } {
        bindings :+= k + ": " + this(v)
      }
      sb.append(bindings.mkString(", "))
      sb.append(") {")
      sb.append(this(expression))
      sb.append("}")
      sb.toString
    }
    case ApplyFunction(func, input) => {
      this(func) + " " + this(input)
    }
    case ParameterObj(name) => name
    case StructType(params) => {
      "Struct (" + this(params) + ")"
    }
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
    case SubtypeType(parentType) => {
      "Subtype(" + this(parentType) + ")"
    }
  }

  def applyType(nType: NewMapType): String = nType match {
    case IndexT(i) => i.toString
    case TypeT => "Type"
    case ObjectT => "Object"
    case IdentifierT => "Identifier"
    case MapT(key, value, default) => {
      "Map (" + applyType(key) + ") => (" + applyType(value) + ") default " + apply(default)
    }
    case ParameterList(params) => {
      val sb: StringBuilder = new StringBuilder()
      sb.append("(")
      var bindings: Vector[String] = Vector.empty
      for {
        (k, v) <- params
      } {
        bindings :+= (k + ": " + applyType(v))
      }
      sb.append(bindings.mkString(", "))
      sb.append(")")
      sb.toString
    }
    case LambdaT(params, result) => {
      val sb: StringBuilder = new StringBuilder()
      sb.append("(")
      var bindings: Vector[String] = Vector.empty
      for {
        (k, v) <- params
      } {
        bindings :+= (k + ": " + applyType(v))
      }
      sb.append(bindings.mkString(", "))
      sb.append(") {")
      sb.append(applyType(result))
      sb.append("}")
      sb.toString
    }
    case SubstitutableT(s) => s
    case Subtype(t) => "Subtype(" + applyType(t) + ")"
  }
}