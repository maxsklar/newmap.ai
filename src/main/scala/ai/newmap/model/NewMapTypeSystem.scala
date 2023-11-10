package ai.newmap.model

import ai.newmap.util.{Outcome, RangeMap, Success, Failure}
import ai.newmap.interpreter.TypeConvertibilityGraph

// Each Type System:
// - has a UUID
// - has a mapping from identifiers to type constructors (and perhaps back)
// Each Type Constructor:
// - Has a Uuid + an identifier
// - Has a type parameter
// - Turns a value in the type parameter to a "type"
// Each Type Has:
// - The type constructor (which is a UUID - but get's mapped to an identifier through the type system)
// - The type parameter (which could itself be a type or contain a type)
// - it doesn't need to have the type system that created it, but for each type constructor,
//   there should be a mapping to a set of type systems that it's a part of
// - A mapping to its underlying type
// There is also:
// - convertibility mappings between types
//   - This mapping is acyclic, but also contains unique paths (it is a tree)
//   - types that convert to each other also contain unique paths
//   - in the future, we'll allow multiple paths to convertibility so long as we can prove that they are equivalent
// - we have a notion of the "current" type system, a uuid
// - we should be able to take a type, do a breadth first search on convertibility, and see if we can map to
//   something in the current type system
case class NewMapTypeSystem(
  currentVersion: Long = 0,

  // Doesn't include all base types
  // defaults to empty struct??
  typeToParameterType: Map[String, RangeMap[NewMapType]] = Map.empty,

  // First item is the pattern for the parameter, and the second item is the type
  typeToUnderlyingType: Map[String, RangeMap[(UntaggedObject, NewMapType)]] = Map(
    "Index" -> RangeMap(0, UWildcard("i") -> IndexT(ParamId("i"))),
    "Array" -> RangeMap(0, UWildcard("T") -> CaseT(
        UMap(Vector(UWildcard("i") ->
         MapT(
           TypeTransform(IndexT(ParamId("i")), ParamIdT("T")),
           MapConfig(RequireCompleteness, SimpleFunction)
         ).asUntagged
        )),
        CountT,
        SimpleFunction
      )
    )
  ),

  // These are the rules to convert from some type to a future version.
  // More flexible conversion rules (for completely different types) still need to be implemented
  forwardCompatibilityRules: Map[String, RangeMap[FunctionWithMatchingRules]] = Map.empty,

  // This is the general convertibility search
  // It is a Map from Type => (Type => convertInstructions)
  convertibilityGraph: TypeConvertibilityGraph = TypeConvertibilityGraph.init
) {
  def emptyStructType = StructT(UMap(Vector.empty), IndexT(UIndex(0)), RequireCompleteness, BasicMap)
  def emptyStructPattern = UArray()

  def getParameterType(
    typeSystemId: Long,
    identifier: String
  ): Outcome[NewMapType, String] = identifier match {
    case "Count" => Success(emptyStructType)
    case "Boolean" => Success(emptyStructType)
    case "Byte" => Success(emptyStructType)
    case "Character" => Success(emptyStructType)
    //case "String" => Success(emptyStructType)
    case "Long" => Success(emptyStructType)
    case "Double" => Success(emptyStructType)
    case "Uuid" => Success(emptyStructType)
    case "UndefinedType" => Success(emptyStructType)
    case "Type" => Success(emptyStructType)
    case "Identifier" => Success(emptyStructType)
    case "Map" => {
      Failure("Map not implemented")
    }
    case "Struct" => Failure("Struct not implemented")
    case "TypeClass" => Failure("TypeClass not implemented")
    case "Case" => Failure("Case not implemented")
    case "Subtype" => Failure("Subtype not implemented")
    case "TypeTransform" => Success(emptyStructType)
    case "Index" => Success(CountT)
    case "Array" => Success(TypeT)
    case custom => {
      for {
        parameterTypeRangeMap <- Outcome(typeToParameterType.get(custom), s"Couldn't get parameterType for identifier $identifier")
        parameterT <- Outcome(parameterTypeRangeMap.get(typeSystemId),  s"Couldn't get parameterType from rangeMap $parameterTypeRangeMap")
      } yield parameterT
    }
  }

  def currentUnderlyingType(name: String): Outcome[(UntaggedObject, NewMapType), String] = {
    for {
      underlyingTypeRangeMap <- Outcome(typeToUnderlyingType.get(name), s"A) Couldn't get underlying type range map $name")

      underlyingType <- Outcome(underlyingTypeRangeMap.get(currentVersion), s"Couldn't get underlying type with current version $currentVersion: $underlyingTypeRangeMap")
    } yield underlyingType
  }

  def historicalUnderlyingType(name: String, version: Long): Outcome[(UntaggedObject, NewMapType), String] = {
    for {
      underlyingTypeRangeMap <- Outcome(typeToUnderlyingType.get(name), s"B) Couldn't get underlying type range map $name")
      underlyingType <- Outcome(underlyingTypeRangeMap.get(version), s"Couldn't get underlying type $version -- $underlyingTypeRangeMap")
    } yield underlyingType
  }

  def getParameterPattern(
    typeSystemId: Long,
    identifier: String
  ): Outcome[UntaggedObject, String] = identifier match {
    case "Count" => Success(emptyStructPattern)
    case "Boolean" => Success(emptyStructPattern)
    case "Byte" => Success(emptyStructPattern)
    case "Character" => Success(emptyStructPattern)
    case "Long" => Success(emptyStructPattern)
    case "Double" => Success(emptyStructPattern)
    case "Uuid" => Success(emptyStructPattern)
    case "UndefinedType" => Success(emptyStructPattern)
    case "Type" => Success(emptyStructPattern)
    case "Identifier" => Success(emptyStructPattern)
    case "Map" => {
      Failure("Map not implemented")
    }
    case "Struct" => Failure("Struct not implemented")
    case "TypeClass" => Failure("TypeClass not implemented")
    case "Case" => Failure("Case not implemented")
    case "Subtype" => Failure("Subtype not implemented")
    case "Array" => Success(UWildcard("T"))
    case custom => historicalUnderlyingType(custom, typeSystemId).map(_._1)
  }

  // Create a new custom type with a given name
  // The parameters for this type (usually will be EMPTY_STRUCT)
  // The starting point 
  def createNewCustomType(
    name: String,
    parameterType: NewMapType,
    parameterPattern: UntaggedObject,
    startingPoint: NewMapType,
  ): Outcome[NewMapTypeSystem, String] = {
    val newVersion: Long = currentVersion + 1

    for {
      _ <- Outcome.failWhen(typeToParameterType.get(name).nonEmpty, s"$name is already a defined type")
    } yield {
      copy(
        currentVersion = newVersion,
        typeToParameterType = typeToParameterType + (name -> RangeMap(newVersion, parameterType)),
        typeToUnderlyingType = typeToUnderlyingType + (name -> RangeMap(newVersion, parameterPattern -> startingPoint))
      )
    }
  }

  // Next: look at the type expansion options, and encode those!
  // - note that we currently don't allow you to change the parameters and parameter pattern
  def upgradeCustomType(
    name: String,
    newUnderlyingT: NewMapType,
    converter: UntaggedObject
  ): Outcome[NewMapTypeSystem, String] = {
    val newVersion: Long = currentVersion + 1

    for {
      currentUnderlyingTypeRangeMap <- Outcome(typeToUnderlyingType.get(name), s"Couldn't find underlying type for $name")
      currentUnderlyingType <- Outcome(currentUnderlyingTypeRangeMap.get(currentVersion), s"Couldn't find underlying type for version $currentVersion in $currentUnderlyingTypeRangeMap")

      currentParameterPattern = currentUnderlyingType._1
    } yield {
      // TODO - this should change based on the input type
      // there needs to be a mapping between the type and the "matcher" it uses
      val matcher = StandardMatcher

      val currentForwardCompatibilityRules = forwardCompatibilityRules.get(name).getOrElse(RangeMap())
      val newForwardCompatibilityRules = currentForwardCompatibilityRules.insert(newVersion, FunctionWithMatchingRules(converter, matcher))

      // Upgrade the appearance of this type everywhere
      val upgradedTypeToUnderlyingType: Map[String, RangeMap[(UntaggedObject, NewMapType)]] = {
        for {
          (k, v) <- typeToUnderlyingType
        } yield {
          val newValueOpt: Option[RangeMap[(UntaggedObject, NewMapType)]] = for {
            underlyingTypeInfo <- v.get(newVersion)
            if (k != name)
            newUnderlying = underlyingTypeInfo._2.upgradeCustom(name, newVersion)
            if (newUnderlying != underlyingTypeInfo._2)
          } yield v.insert(newVersion, underlyingTypeInfo._1 -> newUnderlying)

          val newValue: RangeMap[(UntaggedObject, NewMapType)] = newValueOpt.getOrElse(v)

          k -> newValue
        }
      }

      val newRangeMapForType: RangeMap[(UntaggedObject, NewMapType)] = {
        currentUnderlyingTypeRangeMap.insert(newVersion, (currentParameterPattern -> newUnderlyingT.upgradeCustom(name, newVersion)))
      }

      val newTypeToUnderlyingType: Map[String, RangeMap[(UntaggedObject, NewMapType)]] = {
        upgradedTypeToUnderlyingType + (name -> newRangeMapForType)
      }

      copy(
        currentVersion = newVersion,
        typeToUnderlyingType = newTypeToUnderlyingType,
        forwardCompatibilityRules = forwardCompatibilityRules + (name -> newForwardCompatibilityRules),
      )
    }
  }

  // This will be part of a larger graph search for convertibility!
  // TODO: eventually, when types change names this might get more difficult.
  def searchForForwardConvertibility(
    name: String,
    startingVersion: Long,
    endingVersion: Long
  ): Outcome[Vector[FunctionWithMatchingRules], String] = {
    val rules: RangeMap[FunctionWithMatchingRules] = forwardCompatibilityRules.get(name).getOrElse(RangeMap())

    if (endingVersion >= startingVersion) {
      val valuesBetween = rules.valuesBetween(startingVersion, endingVersion)
      Success(valuesBetween.toVector)
    } else {
      val valuesBetween = rules.valuesBetween(endingVersion, startingVersion)

      if (valuesBetween.isEmpty) Success(Vector.empty)
      else  {
        // In the future, also allow changes that are bijective (such as permutations)
        // Currently, no changes are backwards compatible
        Failure(s"Backwards compatibility not possible or not implemented: $name: $startingVersion -> $endingVersion")
      }
    }
  }
}

object NewMapTypeSystem {
  case class TypeWithVersion(
    typeName: String,
    typeSystemVersion: Long
  )

  val emptyStruct: NewMapType = StructT(UMap(Vector.empty), IndexT(UIndex(0)), RequireCompleteness, BasicMap)

  val baseTypes = Set(
    "Count",
    "Character",
    "Long",
    "Double",
    "Uuid",
    "UndefinedType",
    "Type",
    "Identifier",
    "Map",
    "Struct",
    "TypeClass",
    "Case",
    "Subtype",
    "Array",
    "Sequence"
  )

  def isCustomType(s: String): Boolean = !baseTypes.contains(s)
}