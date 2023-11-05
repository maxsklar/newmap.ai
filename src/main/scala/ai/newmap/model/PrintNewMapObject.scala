package ai.newmap.model

// CAREFUL! This should not be imported here, need to move some things around
import ai.newmap.interpreter.Evaluator

import java.util.UUID
import ai.newmap.util.{Outcome, Success, Failure}

object PrintNewMapObject {
  def apply(nObject: NewMapObject, env: Environment): String = {
    val defaultString = untagged(nObject.uObject)

    printObjectFromEnv(
      nObject.uObject,
      nObject.nType.asUntagged,
      env
    ).toOption.getOrElse(defaultString)
  }

  /*
   * This is the "display" code from a type class being built into newmap.ai
   */
  def printObjectFromEnv(
    uObject: UntaggedObject,
    nType: UntaggedObject,
    env: Environment
  ): Outcome[String, String] = {
    for {
      displayUnderlyingType <- env.typeSystem.currentUnderlyingType("Displayable")

      dislayUnderlyingExpT = displayUnderlyingType._2

      // TODO - given the new TypeClassT, the rest of this is nonsense
      mapValues <- dislayUnderlyingExpT match {
        case TypeClassT(typeSet) => Success(typeSet)
        case _ => Failure(s"Not a type class: ${dislayUnderlyingExpT.displayString(env)}")
      }

      membershipCheck <- Evaluator.applyFunction(mapValues, nType, env, TypeMatcher)
      _ <- Outcome.failWhen(membershipCheck == UInit, s"Not member of Displayable typeclass: $nType")

      evalResult <- Evaluator(AccessField(uObject, nType, UIdentifier("display")), env)

      finalString <-  evalResult match {
        case UCase(_, UArray(chars)) => {
          Success(chars.mkString)
        }
        case _ => Failure(s"Couldn't interpret result as string: $evalResult")
      }
    } yield finalString
  }

  // TODO - convert nType into an untagged type instead of using this!
  def newMapType(
    nType: NewMapType,
    typeSystem: NewMapTypeSystem
  ): String = {
    nType match {
      case CountT => "Count"
      case IndexT(i) => "Index." + untagged(i)
      case CustomT(name, params, typeSystemState) => {
        val legacyIndicator = if (typeSystemState == typeSystem.currentVersion) s"[CUR:$typeSystemState]" else s"[OLD:$typeSystemState]"
        val includedParams = if (isEmptyMap(params)) "" else s"|${untagged(params)}"

        s"${legacyIndicator}${name}$includedParams"
      }
      case TypeT => s"Type"
      case UndefinedT => s"UndefinedType"
      case IdentifierT => "Identifier"
      case BooleanT => "Boolean"
      case ByteT => "Byte"
      case CharacterT  => "Character"
      case LongT => "Long"
      case DoubleT => "Double"
      case UuidT => "UUID"
      case TypeTransformT(_) => "TypeTransform"
      case MapT(TypeTransform(key, value), config) => {
        printMapT(
          newMapType(key, typeSystem),
          newMapType(value, typeSystem),
          config
        )
      }
      //case StructT(params, parentType, completeness, featureSet) => s"Struct(${mapToString(params)})~$parentType~$completeness~$featureSet"
      case StructT(params, _, _, _) => s"Struct(${untagged(params)})"
      case TypeClassT(typeSet) => {
        s"TypeClassT(${untagged(typeSet)})"
      }
      case CaseT(cases, _, _) => {
        s"Case${untagged(cases)}"
      }
      case SequenceT(parent, _) => {
        s"Sequence(${newMapType(parent, typeSystem)})"
      }
      //TODO(2022): we might not want to print out the full parent here, because it could be large
      // - instead, we link to the function or map somehow... when we give things uniqueids we can figure this out
      case SubtypeT(isMember, _, _) => s"Subtype(${untagged(isMember)})"
      case FunctionalSystemT(functionTypes) => s"FunctionalSystem(${untagged(UMap(functionTypes))})"
      case ArrayT(t) => "Array|" + newMapType(t, typeSystem)
      case WildcardT(name) => untagged(UWildcard(name))
      case ParamIdT(name) => untagged(ParamId(name))
    }
  }

  def printMapT(
    key: String,
    value: String,
    config: MapConfig
  ): String = {
    (config.completeness, config.featureSet) match {
      case (CommandOutput, BasicMap) => "Map(" + key + ": " + value + ")"
      case (RequireCompleteness, SimpleFunction) => "ReqMap(" + key + ": " + value + ")"
      case (RequireCompleteness, FullFunction) => {
        // TODO(2022): Change the way lambda input works so that it's more like Map
        "(" + key + " => " + value + ")"
      }
      case _ => {
        val completenessStr = config.completeness match {
          case RequireCompleteness => "Required"
          case CommandOutput => "Command"
          case PartialMap => "Partial"
        }

        val featureSetStr = config.featureSet match {
          case BasicMap => "BasicMap"
          case PatternMap => "PatternMap"
          case SimpleFunction => "Simple"
          case WellFoundedFunction => "WellFounded"
          case FullFunction => "FullFunction"
        }

        // TODO(2022): Improve Notation so that we don't need this!
        s"Map($key: $value, $completenessStr, $featureSetStr)"
      }  
    }
  }

  def isEmptyMap(value: UntaggedObject): Boolean = value match {
    case UArray(v) => v.isEmpty
    case UMap(v) => v.isEmpty
    case _ => false
  }

  def untagged(uObject: UntaggedObject): String = uObject match {
    case UIdentifier(s) => s
    case UMap(values) => mapToString(values)
    case USingularMap(key, value) => "UMP~" + mapToString(Vector(key -> value))
    case UArray(values) => sequenceToString(values)
    case UCase(constructor, value) => {
      if (isEmptyMap(value)) {
        untagged(constructor)
      } else {
        untagged(constructor) + "|" + untagged(value)
      }
    }
    case UIndex(i) => i.toString
    case UInit => "()"
    case IsCommandFunc => s"IsCommandFunc"
    case ULink(key) => {
      s"ULink[${key.toString}]"
    }
    case UByte(value: Byte) => s"$value"
    case UCharacter(value: Char) => s"$value"
    case ULet(envCommands, nObject) => s"{${envCommands.mkString("; ")}; ${untagged(nObject)}"
    case ULong(value: Long) => s"$value"
    case UDouble(value: Double) => s"$value"
    case Uuuid(value) => s"$value"
    case UFunctionLink(functionName, functionalSystem) => {
      s"${untagged(functionName)}~$functionalSystem}"
    }
    case UWildcard(name) => "W~" + name
    case ParamId(name) => s"$name~pi"
    case ApplyFunction(func, input, _) => {
      "(" + untagged(func) + " " + untagged(input) + ")"
    }
    case AccessField(value@UIndex(_), _, field) => "(" + untagged(value) + ")." + untagged(field)
    case AccessField(value@ULong(_), _, field) => "(" + untagged(value) + ")." + untagged(field)
    case AccessField(value@UDouble(_), _, field) => "(" + untagged(value) + ")." + untagged(field)
    case AccessField(value, _, field) => untagged(value) + "." + untagged(field)
    case UPlus => "+"
    case UTimes => "*"
    case UDivide => "/"
    //case UMinus => "-"
    case UCountToDecimal => "CountToDecimal"
  }

  def mapToString(values: Vector[(UntaggedObject, UntaggedObject)]): String = {
    val sb: StringBuilder = new StringBuilder()
    sb.append("(")

    var bindings: Vector[String] = Vector.empty
    for {
      (k, v) <- values
    } {
      bindings :+= untagged(k) + ": " + untagged(v)
    }
    sb.append(bindings.mkString(", "))

    sb.append(")")
    sb.toString
  }

  def sequenceToString(values: Seq[UntaggedObject]): String = {
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