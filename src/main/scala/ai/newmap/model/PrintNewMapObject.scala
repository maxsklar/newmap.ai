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
      displayTypeId <- Outcome(env.typeSystem.currentMapping.get("_display"), "Couldn't find _display typeClass")

      displayUnderlyingType <- Outcome(env.typeSystem.typeToUnderlyingType.get(displayTypeId), "Couldn't find _display underlying typeclass")

      displayParameterPattern = displayUnderlyingType._1
      dislayUnderlyingExpT <- displayUnderlyingType._2.asType

      mapValues <- dislayUnderlyingExpT match {
        case TypeClassT(_, values) => Success(values)
        case _ => Failure(s"Not a type class: ${dislayUnderlyingExpT.displayString(env)}")
      }

      result <- Evaluator.applyFunction(mapValues, nType, env, TypeMatcher) match {
        case Success(s) => Success(s)
        case Failure(f) => Failure(f.toString)
      }

      evalResult <- Evaluator(ApplyFunction(result, uObject, StandardMatcher), env)

      finalString <-  evalResult match {
        case UCase(_, UStruct(chars)) => {
          Success(chars.mkString)
        }
        case _ => Failure(s"Couldn't interpret result as string: $evalResult")
      }
    } yield finalString
  }

  // TODO - convert nType into an untagged type instead of using this!
  def newMapType(
    nType: NewMapType,
    typeSystem: NewMapTypeSystem,
    typeSystemStateOpt: Option[UUID] = None
  ): String = {
    val typeSystemState = typeSystemStateOpt.getOrElse(typeSystem.currentState)

    nType match {
      case CountT => "Count"
      case IndexT(i) => "Index." + untagged(i)
      case CustomT(name, params) => {
        val resultOpt: Option[String] = for {
          currentId <- typeSystem.currentMapping.get(name)
          referencedId <- typeSystem.historicalMapping.get(typeSystemState).getOrElse(Map.empty).get(name)
        } yield {
          val legacyIndicator = if (currentId == referencedId) "" else s"[OLD:$referencedId]"
          val includedParams = if (isEmptyMap(params)) "" else s"|${untagged(params)}"

          s"${legacyIndicator}${name}$includedParams"
        }

        resultOpt match {
          case Some(result) => result
          case None => {
            s"[Failed to print type: $name -- $typeSystemState -- ${typeSystem.currentState} -- ${typeSystem.currentMapping.get(name)} -- ${typeSystem.historicalMapping.get(typeSystemState)}]"
          }
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
      //case StringT => "String"
      case LongT => "Long"
      case DoubleT => "Double"
      case UuidT => "UUID"
      case TypeTransformT => "TypeTransform"
      case MapT(TypeTransform(key, value), config) => {
        printMapT(
          newMapType(key, typeSystem, typeSystemStateOpt),
          newMapType(value, typeSystem, typeSystemStateOpt),
          config
        )
      }
      //case StructT(params, parentType, completeness, featureSet) => s"Struct(${mapToString(params)})~$parentType~$completeness~$featureSet"
      case StructT(params, _, _, _) => s"Struct(${untagged(params)})"
      case TypeClassT(typeTransform, implementation) => {
        s"TypeClassT(${untagged(typeTransform)}, ${untagged(implementation)})"
      }
      case CaseT(cases, _, _) => {
        s"Case${untagged(cases)}"
      }
      //TODO(2022): we might not want to print out the full parent here, because it could be large
      // - instead, we link to the function or map somehow... when we give things uniqueids we can figure this out
      case SubtypeT(isMember, _, _) => s"Subtype(${untagged(isMember)})"
      case FunctionalSystemT(functionTypes) => s"FunctionalSystem(${untagged(UMap(functionTypes))})"
      case WithStateT(uuid, nType) => {
        newMapType(nType, typeSystem, Some(uuid))
        /*if (typeSystem.currentState == uuid) {
          newMapType(nType, typeSystem)
        } else {
          println(s"*** type system uuid: ${typeSystem.currentState}")

          s"WithState:$uuid:${newMapType(nType, typeSystem)}" 
        }*/
      }
      case WildcardPatternT(name) => untagged(UWildcardPattern(name))
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
          case MapPattern => "Pattern"
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
    case UStruct(v) => v.isEmpty
    case UMap(v) => v.isEmpty
    case _ => false
  }

  def untagged(uObject: UntaggedObject): String = uObject match {
    case UIdentifier(s) => s
    case UMap(values) => mapToString(values)
    case UMapPattern(key, value) => mapToString(Vector(key -> value))
    case UStruct(values) => sequenceToString(values)
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
    //case UString(value) => s"$value~str"
    case ULet(envCommands, nObject) => s"{${envCommands.mkString("; ")}; ${untagged(nObject)}"
    case ULong(value: Long) => s"$value"
    case UDouble(value: Double) => s"$value"
    case Uuuid(value) => s"$value"
    case UFunctionLink(functionName, functionalSystem) => {
      s"${untagged(functionName)}~$functionalSystem}"
    }
    case UWildcardPattern(name) => "W~" + name
    case ParamId(name) => s"$name~pi"
    case ApplyFunction(func, input, _) => {
      "(" + untagged(func) + " " + untagged(input) + ")"
    }
    case AccessField(value@UIndex(_), _, field) => "(" + untagged(value) + ")." + untagged(field)
    case AccessField(value@ULong(_), _, field) => "(" + untagged(value) + ")." + untagged(field)
    case AccessField(value@UDouble(_), _, field) => "(" + untagged(value) + ")." + untagged(field)
    case AccessField(value, _, field) => untagged(value) + "." + untagged(field)
    case UPlus => "+"
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