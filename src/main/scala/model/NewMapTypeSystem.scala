package ai.newmap.model

import java.util.UUID
import ai.newmap.util.{Outcome, Success, Failure}

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
  currentState: NewMapTypeSystem.Id,
  historicalMapping: Map[NewMapTypeSystem.Id, Map[String, NewMapTypeSystem.TypeId]],

  // Doesn't include all base types
  // defaults to empty struct??
  typeToParameterType: Map[NewMapTypeSystem.TypeId, UntaggedObject] = Map(
    NewMapTypeSystem.indexTid -> UCase(UIdentifier("Count"), UMap(Vector.empty))
  ),

  // First item is the pattern for the parameter, and the second item is the type
  typeToUnderlyingType: Map[NewMapTypeSystem.TypeId, (UntaggedObject, UntaggedObject)] = Map(
    NewMapTypeSystem.indexTid -> (UWildcardPattern("i"), UCase(UIdentifier("Index"), UParamId("i"))) 
  ),

  // For each type constructor, I want to know which other type constructors are using this in their parameter
  // If we update that type, all of the types downstream need to be updated as well!!
  // Doesn't include the
  typesThatUseType: Map[NewMapTypeSystem.TypeId, Vector[NewMapTypeSystem.TypeId]] = Map.empty,

  // Doesn't include conversion rules for base types like Map and Struct
  // Maybe it will in the future! It'd save us some code in SubtypeUtils.scala
  // Parameters: Starting Type, Ending Type
  // Result: Partial Function: Starting Parameter => (Ending Parameter => ConvertibilityRules)
  // Convertibility rule is a complete function from StartingType => EndingType (must be 1:1)
  customConvertibilityRules: Map[(NewMapTypeSystem.TypeId, NewMapTypeSystem.TypeId), UntaggedObject] = Map.empty
) {
  def currentMapping: Map[String, NewMapTypeSystem.TypeId] = {
    historicalMapping.getOrElse(currentState, Map.empty)
  }

  def typeToUntaggedObject(nType: NewMapType): UntaggedObject = nType match {
    case CountT => UCase(UIdentifier("Count"), UStruct(Vector.empty))
    case IndexT(i) => UCase(UIdentifier("Index"), UIndex(i))
    case BooleanT => UCase(UIdentifier("Count"), UStruct(Vector.empty))
    case ByteT => UCase(UIdentifier("Byte"), UStruct(Vector.empty))
    case CharacterT => UCase(UIdentifier("Character"), UStruct(Vector.empty))
    case StringT => UCase(UIdentifier("String"), UStruct(Vector.empty))
    case LongT => UCase(UIdentifier("Long"), UStruct(Vector.empty))
    case DoubleT => UCase(UIdentifier("Double"), UStruct(Vector.empty))
    case UuidT => UCase(UIdentifier("Uuid"), UStruct(Vector.empty))
    case UndefinedT => UCase(UIdentifier("UndefinedType"), UStruct(Vector.empty))
    case TypeT => UCase(UIdentifier("Type"), UStruct(Vector.empty))
    case HistoricalTypeT(uuid) => UCase(UIdentifier("HistoricalType"), Uuuid(uuid))
    case IdentifierT => UCase(UIdentifier("Identifier"), UStruct(Vector.empty))
    case MapT(typeTransform, config) => UCase(UIdentifier("Map"), UStruct(Vector(
      UMap(typeTransform),
      UStruct(Vector(
        UIdentifier(config.completeness.getName),
        UIdentifier(config.featureSet.getName),
        UStruct(config.preservationRules.map(_.toUntaggedObject)) // This should be a map in the future, not a struct
      ))
    )))
    case StructT(params, fieldParentType, completenesss, featureSet) => UCase(UIdentifier("Struct"), UStruct(Vector(
      UMap(params),
      typeToUntaggedObject(fieldParentType),
      UIdentifier(completenesss.getName),
      UIdentifier(featureSet.getName)
    )))
    case TypeClassT(typeTransform, typesInTypeClass) => UCase(UIdentifier("TypeClass"), UStruct(Vector(
      UMap(typeTransform),
      UStruct(typesInTypeClass)
    )))
    case CaseT(cases, fieldParentType, featureSet) => UCase(UIdentifier("Case"), UStruct(Vector(
      UMap(cases),
      typeToUntaggedObject(fieldParentType),
      UIdentifier(featureSet.getName)
    )))
    case SubtypeT(isMember, parentType, featureSet) => UCase(UIdentifier("Subtype"), UStruct(Vector(
      isMember,
      typeToUntaggedObject(parentType),
      UIdentifier(featureSet.getName)
    )))
    case CustomT(name, params) => UCase(UIdentifier(name), params)
    case WithStateT(uuid, nType) => {
      UCase(
        UIdentifier("WithState"),
        UCase(
          Uuuid(uuid),
          typeToUntaggedObject(nType)
        )
      )
    }
    case WildcardPatternT(name) => UWildcardPattern(name)
  }

  def emptyStructType = StructT(Vector.empty, IndexT(0), RequireCompleteness, BasicMap)
  def emptyStructPattern = UStruct(Vector.empty)

  def getParameterType(
    typeSystemId: NewMapTypeSystem.Id,
    identifier: String
  ): Outcome[NewMapType, String] = identifier match {
    case "Count" => Success(emptyStructType)
    case "Boolean" => Success(emptyStructType)
    case "Byte" => Success(emptyStructType)
    case "Character" => Success(emptyStructType)
    case "String" => Success(emptyStructType)
    case "Long" => Success(emptyStructType)
    case "Double" => Success(emptyStructType)
    case "Uuid" => Success(emptyStructType)
    case "UndefinedType" => Success(emptyStructType)
    case "Type" => Success(emptyStructType)
    case "HistoricalType" => Success(UuidT)
    case "Identifier" => Success(emptyStructType)
    case "Map" => {
      Failure("Map not implemented")
      /*val uTypeT = typeToUntaggedObject(TypeT)
      val standardTypeTransform = Vector(uTypeT -> ObjectExpression(uTypeT))
      Success(buildStructTypeFromParamList(Vector(
        typeToUntaggedObject(MapT(standardTypeTransform, MapConfig(CommandOutput, SimpleFunction))),
        /// config???
      )))*/
    }
    case "Struct" => Failure("Struct not implemented")
    case "TypeClass" => Failure("TypeClass not implemented")
    case "Case" => Failure("Case not implemented")
    case "Subtype" => Failure("Subtype not implemented")
    case "WithState" => Success(
      CaseT(
        Vector(UWildcardPattern("tsid") -> BuildCase(UIdentifier("HistoricalType"), ParamId("tsid"))),
        UuidT,
        SimpleFunction
      )
    )
    case custom => {
      for {
        identifierToIdMapping <- Outcome(historicalMapping.get(typeSystemId), s"Couldn't get historical type mapping for $typeSystemId")
        typeId <- Outcome(identifierToIdMapping.get(identifier), s"Couldn't get typeId for identifier $identifier")
        parameterType <- Outcome(typeToParameterType.get(typeId), s"Couldn't get parameterType for identifier $identifier")
        parameterT <- convertToNewMapType(parameterType)
      } yield parameterT
    }
  }

  def getParameterPattern(
    typeSystemId: NewMapTypeSystem.Id,
    identifier: String
  ): Outcome[UntaggedObject, String] = identifier match {
    case "Count" => Success(emptyStructPattern)
    case "Boolean" => Success(emptyStructPattern)
    case "Byte" => Success(emptyStructPattern)
    case "Character" => Success(emptyStructPattern)
    case "String" => Success(emptyStructPattern)
    case "Long" => Success(emptyStructPattern)
    case "Double" => Success(emptyStructPattern)
    case "Uuid" => Success(emptyStructPattern)
    case "UndefinedType" => Success(emptyStructPattern)
    case "Type" => Success(emptyStructPattern)
    case "HistoricalType" => Success(UWildcardPattern("typeSystemId"))
    case "Identifier" => Success(emptyStructPattern)
    case "Map" => {
      Failure("Map not implemented")
      /*val uTypeT = typeToUntaggedObject(TypeT)
      val standardTypeTransform = Vector(uTypeT -> ObjectExpression(uTypeT))
      Success(buildStructTypeFromParamList(Vector(
        typeToUntaggedObject(MapT(standardTypeTransform, MapConfig(CommandOutput, SimpleFunction))),
        /// config???
      )))*/
    }
    case "Struct" => Failure("Struct not implemented")
    case "TypeClass" => Failure("TypeClass not implemented")
    case "Case" => Failure("Case not implemented")
    case "Subtype" => Failure("Subtype not implemented")
    case "WithState" => Failure("WithState not implemented")
    case custom => {
      for {
        identifierToIdMapping <- Outcome(historicalMapping.get(typeSystemId), s"Couldn't get historical type mapping for $typeSystemId")
        typeId <- Outcome(identifierToIdMapping.get(identifier), s"Couldn't get typeId for identifier $identifier")
        underlyingType <- Outcome(typeToUnderlyingType.get(typeId), s"Couldn't get underlying type for identifier $identifier")
      } yield underlyingType._1
    }
  }

  def buildStructTypeFromParamList(paramList: Vector[UntaggedObject]): NewMapType = {
    StructT(
      paramList.zipWithIndex.map(x => UIndex(x._2) -> ObjectExpression(x._1)),
      IndexT(paramList.length)
    )
  }

  // Should this be required to contain the uuid?
  def convertToNewMapType(
    uType: UntaggedObject
  ): Outcome[NewMapType, String] = uType match {
    case UCase(UIdentifier(identifier), params) => identifier match {
      case "Count" => Success(CountT)
      case "Index" => params match {
        case UIndex(i) => Success(IndexT(i))
        case _ => Failure(s"Couldn't convert index to NewMapType with params: $params")
      }
      case "Boolean" => Success(BooleanT)
      case "Byte" => Success(ByteT)
      case "Character" => Success(CharacterT)
      case "String" => Success(StringT)
      case "Long" => Success(LongT)
      case "Double" => Success(DoubleT)
      case "Uuid" => Success(UuidT)
      case "Undefined" => Success(UndefinedT)
      case "Type" => Success(TypeT)
      case "HistoricalType" => params match {
        case Uuuid(uuid) => Success(HistoricalTypeT(uuid))
        case _ => Failure(s"Couldn't convert HistoricalType to NewMapType with params: $params")
      }
      case "Identifier" => Success(IdentifierT)
      case "Map" => params match {
        case UStruct(items) if (items.length == 2) => {
          for {
            typeTransform <- items(0) match {
              case UMap(v) => Success(v)
              case _ => Failure(s"Incorrect type transform: ${items(0)}")
            }

            config <- items(1) match {
              case UStruct(v) if (v.length == 3) => Success(v)
              case _ => Failure(s"Incorrect config: ${items(1)}")
            }

            completeness <- config(0) match {
              case UIdentifier("RequireCompleteness") => Success(RequireCompleteness)
              case UIdentifier("CommandOutput") => Success(CommandOutput)
              case _ => Failure(s"Can't allow feature set in struct type: ${items(1)}")
            }

            featureSet <- convertFeatureSet(config(1))

            // TODO: config(2) for the preservation rules
          } yield {
            MapT(typeTransform, MapConfig(completeness, featureSet))
          }
        }
        case _ => Failure(s"Couldn't convert Map to NewMapType with params: $params")
      }
      case "Struct" => params match {
        case UStruct(items) if (items.length == 4) => {
          for {
            structMap <- items(0) match {
              case UMap(uMap) => Success(uMap)
              case _ => Failure(s"Invalid struct map: ${items(0)}")
            }

            parentT <- convertToNewMapType(items(1))

            completeness <- items(2) match {
              case UIdentifier("RequireCompleteness") => Success(RequireCompleteness)
              case UIdentifier("CommandOutput") => Success(CommandOutput)
              case _ => Failure(s"Can't allow feature set in struct type: ${items(2)}")
            }

            featureSet <- convertFeatureSet(items(3))
          } yield {
            StructT(structMap, parentT, completeness, featureSet)
          }
        }
        case _ => Failure(s"Couldn't convert Struct to NewMapType with params: $params")
      }
      case "TypeClass" => params match {
        case _ => Failure(s"Couldn't convert TypeClass to NewMapType with params: $params")
      }
      case "Case" => params match {
        case UStruct(items) if items.length == 3 => {
          for {
            caseMap <- items(0) match {
              case UMap(uMap) => Success(uMap)
              case _ => Failure(s"Invalid case map: ${items(0)}")
            }

            parentT <- convertToNewMapType(items(1))

            featureSet <- convertFeatureSet(items(2))
          } yield CaseT(caseMap, parentT, featureSet)
        }
        case _ => Failure(s"Couldn't convert Case to NewMapType with params: $params")
      }
      case "Subtype" => params match {
        case UStruct(items) if items.length == 3 => {
          for {
            parentT <- convertToNewMapType(items(1))

            featureSet <- items(2) match {
              case UIdentifier("BasicMap") => Success(BasicMap)
              case UIdentifier("SimpleFunction") => Success(SimpleFunction)
              case _ => Failure(s"Can't allow feature set in subtype: ${items(2)}")
            }
          } yield SubtypeT(items(0), parentT, featureSet)
        }
        case _ => Failure(s"Couldn't convert Subtype to NewMapType with params: $params")
      }
      case "WithState" => params match {
        case UCase(Uuuid(uuid), t) => {
          for {
            resultT <- convertToNewMapType(t)
          } yield {
            WithStateT(uuid, resultT)
          }
        }
        case _ => Failure(s"Couldn't convert WithState tp NewMapType with params: $params")
      }
      case custom => Success(CustomT(custom, params))
    }
    case UInit => Success(UndefinedT)
    case UIndex(i) => Success(IndexT(i))
    case UIdentifier(name) => convertToNewMapType(UCase(UIdentifier(name), UInit))
    case UWildcardPattern(name) => Success(WildcardPatternT(name))
    case _ => {
      Failure(s"Couldn't convert to NewMapType: $uType")
    }
  }

  def convertFeatureSet(uObject: UntaggedObject): Outcome[MapFeatureSet, String] = uObject match {
    case UIdentifier("BasicMap") => Success(BasicMap)
    case UIdentifier("SimpleFunction") => Success(SimpleFunction)
    case UIdentifier("WellFoundedFunction") => Success(WellFoundedFunction)
    case UIdentifier("FullFunction") => Success(FullFunction)
    case _ => Failure(s"Can't allow feature set in struct type: $uObject")
  }

  def getTypeIdFromName(typeSystemId: NewMapTypeSystem.Id, name: String): Outcome[NewMapTypeSystem.TypeId, String] = {
    for {
      mapping <- Outcome(historicalMapping.get(typeSystemId), s"Type mapping for id $typeSystemId not found")
      typeId <- Outcome(mapping.get(name), s"TypeId for name $name not found")
    } yield typeId
  }

    // Create a new custom type with a given name
  // The parameters for this type (usually will be EMPTY_STRUCT)
  // The starting point 
  def createNewCustomType(
    name: String,
    parameterType: UntaggedObject,
    parameterPattern: UntaggedObject,
    startingPoint: UntaggedObject,
  ): Outcome[NewMapTypeSystem, String] = {
    val newCustomTypeId: NewMapTypeSystem.TypeId = java.util.UUID.randomUUID
    val newTypeSystemId: NewMapTypeSystem.Id = java.util.UUID.randomUUID

    for {
      currentMapping <- Outcome(historicalMapping.get(currentState), s"Current type mapping $currentState not found")
      _ <- Outcome.failWhen(currentMapping.get(name).nonEmpty, s"$name is already a defined type")
    } yield {
      // TODO: implement
      val newTypesThatAreUsed = typesThatUseType

      copy(
        currentState = newTypeSystemId,
        historicalMapping = historicalMapping + (newTypeSystemId -> (currentMapping + (name -> newCustomTypeId))),
        typeToParameterType = typeToParameterType + (newCustomTypeId -> parameterType),
        typeToUnderlyingType = typeToUnderlyingType + (newCustomTypeId -> (parameterPattern, startingPoint)),
        typesThatUseType = newTypesThatAreUsed
      )
    }
  }

  // Next: look at the type expansion options, and encode those!
  // - note that we currently don't allow you to change the parameters and parameter pattern
  def upgradeCustomType(
    name: String,
    newUnderlyingType: UntaggedObject,
    converter: UntaggedObject
  ): Outcome[NewMapTypeSystem, String] = {
    val newTypeId: NewMapTypeSystem.TypeId = java.util.UUID.randomUUID
    val newTypeSystemId: NewMapTypeSystem.Id = java.util.UUID.randomUUID

    for {
      currentMapping <- Outcome(historicalMapping.get(currentState), s"Current type mapping $currentState not found")
      currentTypeId <- Outcome(currentMapping.get(name), s"$name must be defined")
      currentUnderlyingType <- Outcome(typeToUnderlyingType.get(currentTypeId), s"Couldn't find underlying type for $name")
      
      // This doesn't change, but for now needs to be copied
      currentParameterType <- Outcome(typeToParameterType.get(currentTypeId), s"Couldn't find current type parameter for $name")
      
      currentParameterPattern = currentUnderlyingType._1
    } yield {
      // TODO - add others from typesThatUseType
      val newMapping = currentMapping + (name -> newTypeId)

      // TODO: implement
      val newTypesThatAreUsed = typesThatUseType

      copy(
        currentState = newTypeSystemId,
        historicalMapping = historicalMapping + (newTypeSystemId -> newMapping),
        typeToParameterType = typeToParameterType + (newTypeId -> currentParameterType),
        typeToUnderlyingType = typeToUnderlyingType + (newTypeId -> (currentParameterPattern -> newUnderlyingType)),
        customConvertibilityRules = customConvertibilityRules + ((currentTypeId -> newTypeId) -> converter),
        typesThatUseType = newTypesThatAreUsed
      )
    }
  }

  // Search for convertibility using a simple graph search
  // In the future - improve this algo!!
  def searchForConvertibility(
    typeId1: NewMapTypeSystem.TypeId,
    typeId2: NewMapTypeSystem.TypeId
  ): Outcome[Vector[UntaggedObject], String] = {
    val convertibilityRules: Vector[(NewMapTypeSystem.TypeId, NewMapTypeSystem.TypeId, UntaggedObject)] = {
      customConvertibilityRules.toVector.map(x => {
        (x._1._1, x._1._2, x._2)
      })
    }

    // DO A BFS
    var foundConversions: Map[NewMapTypeSystem.TypeId, Vector[UntaggedObject]] = Map(typeId1 -> Vector.empty)

    var processedTypes: Vector[NewMapTypeSystem.TypeId] = Vector.empty
    var unprocessedTypes: Vector[NewMapTypeSystem.TypeId] = Vector(typeId1)

    while (unprocessedTypes.nonEmpty) {
      val firstType = unprocessedTypes.head

      for {
        applicableRule <- convertibilityRules.filter(_._1 == firstType)

        foundType = applicableRule._2
        if (!processedTypes.contains(foundType))
        if (!unprocessedTypes.contains(foundType))
      } {
        val oldConversionRule: Vector[UntaggedObject] = foundConversions.get(firstType).get
        val newConversionRule = oldConversionRule :+ applicableRule._3
        foundConversions = foundConversions + (foundType -> newConversionRule)
        unprocessedTypes = unprocessedTypes :+ foundType
      }

      unprocessedTypes = unprocessedTypes.drop(1)
    }

    Outcome(foundConversions.get(typeId2), s"Convertibility not found: $typeId1 -- $typeId2\n -- $foundConversions\n")
  }
}

object NewMapTypeSystem {
  type Id = UUID
  type TypeId = UUID

  // Should we generate these a different way - maybe make them standardized?
  val baseState = java.util.UUID.randomUUID
  val indexTid = java.util.UUID.randomUUID

  val initTypeSystem = NewMapTypeSystem(
    currentState = baseState,
    historicalMapping = Map(
      baseState -> Map(
        "Count" -> java.util.UUID.randomUUID,
        "Index" -> indexTid,

        "Boolean" -> java.util.UUID.randomUUID,
        "Byte" -> java.util.UUID.randomUUID,
        "Character" -> java.util.UUID.randomUUID,
        "String" -> java.util.UUID.randomUUID,
        "Long" -> java.util.UUID.randomUUID,
        "Double" -> java.util.UUID.randomUUID,
        "Uuit" -> java.util.UUID.randomUUID,

        "Type" -> java.util.UUID.randomUUID,
        "HistoricalType" -> java.util.UUID.randomUUID,
        "Undefined" -> java.util.UUID.randomUUID,
        "Identifier" -> java.util.UUID.randomUUID,

        "Map" -> java.util.UUID.randomUUID,
        "Struct" -> java.util.UUID.randomUUID,
        "Case" -> java.util.UUID.randomUUID,
        "Subtype" -> java.util.UUID.randomUUID
      )
    ),
  )

  val emptyStruct: NewMapType = StructT(Vector.empty, IndexT(0), RequireCompleteness, BasicMap)
}