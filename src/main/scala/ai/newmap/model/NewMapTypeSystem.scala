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
    NewMapTypeSystem.indexTid -> (UWildcardPattern("i"), UCase(UIdentifier("Index"), ParamId("i"))) 
  ),

  // For each type constructor, I want to know which other type constructors are using this in their parameter
  // If we update that type, all of the types downstream need to be updated as well!!
  // Doesn't include the
  typesThatUseType: Map[NewMapTypeSystem.TypeId, Vector[NewMapTypeSystem.TypeId]] = Map.empty,

  // Doesn't include conversion rules for base types like Map and Struct
  // Maybe it will in the future! It'd save us some code in TypeConversionCalculator.scala
  // Parameters: Starting Type, Ending Type
  // Result: Partial Function: Starting Parameter => (Ending Parameter => ConvertibilityRules)
  // Convertibility rule is a complete function from StartingType => EndingType (must be 1:1)
  customConvertibilityRules: Map[(NewMapTypeSystem.TypeId, NewMapTypeSystem.TypeId), FunctionWithMatchingRules] = Map.empty
) {
  def currentMapping: Map[String, NewMapTypeSystem.TypeId] = {
    historicalMapping.getOrElse(currentState, Map.empty)
  }

  def emptyStructType = StructT(UMap(Vector.empty), IndexT(UIndex(0)), RequireCompleteness, BasicMap)
  def emptyStructPattern = UStruct(Vector.empty)

  def getParameterType(
    typeSystemId: NewMapTypeSystem.Id,
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
    case "HistoricalType" => Success(UuidT)
    case "Identifier" => Success(emptyStructType)
    case "Map" => {
      Failure("Map not implemented")
      /*val uTypeT = typeToUntaggedObject(TypeT)
      val standardTypeTransform = Vector(uTypeT -> uTypeT)
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
        UMap(Vector(UWildcardPattern("tsid") -> UCase(UIdentifier("HistoricalType"), ParamId("tsid")))),
        UuidT,
        SimpleFunction
      )
    )
    case "TypeTransform" => Success(emptyStructType)
    case _ => {
      for {
        identifierToIdMapping <- Outcome(historicalMapping.get(typeSystemId), s"Couldn't get historical type mapping for $typeSystemId")

        typeId <- Outcome(identifierToIdMapping.get(identifier), s"Couldn't get typeId for identifier $identifier")
        parameterType <- Outcome(typeToParameterType.get(typeId), s"Couldn't get parameterType for identifier $identifier")
        parameterT <- parameterType.asType
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
    //case "String" => Success(emptyStructPattern)
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
      val standardTypeTransform = Vector(uTypeT -> uTypeT)
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
    case _ => {
      for {
        identifierToIdMapping <- Outcome(historicalMapping.get(typeSystemId), s"Couldn't get historical type mapping for $typeSystemId")
        typeId <- Outcome(identifierToIdMapping.get(identifier), s"Couldn't get typeId for identifier $identifier")
        underlyingType <- Outcome(typeToUnderlyingType.get(typeId), s"Couldn't get underlying type for identifier $identifier")
      } yield underlyingType._1
    }
  }

  def buildStructTypeFromParamList(paramList: Vector[UntaggedObject]): NewMapType = {
    StructT(
      UMap(paramList.zipWithIndex.map(x => UIndex(x._2) -> x._1)),
      IndexT(UIndex(paramList.length))
    )
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

      // TODO - this should change based on the input type
      // there needs to be a mapping between the type and the "matcher" it uses
      val matcher = StandardMatcher

      copy(
        currentState = newTypeSystemId,
        historicalMapping = historicalMapping + (newTypeSystemId -> newMapping),
        typeToParameterType = typeToParameterType + (newTypeId -> currentParameterType),
        typeToUnderlyingType = typeToUnderlyingType + (newTypeId -> (currentParameterPattern -> newUnderlyingType)),
        customConvertibilityRules = customConvertibilityRules + ((currentTypeId -> newTypeId) -> FunctionWithMatchingRules(converter, matcher)),
        typesThatUseType = newTypesThatAreUsed
      )
    }
  }

  // Search for convertibility using a simple graph search
  // In the future - improve this algo!!
  def searchForConvertibility(
    typeId1: NewMapTypeSystem.TypeId,
    typeId2: NewMapTypeSystem.TypeId
  ): Outcome[Vector[FunctionWithMatchingRules], String] = {
    val convertibilityRules: Vector[(NewMapTypeSystem.TypeId, NewMapTypeSystem.TypeId, FunctionWithMatchingRules)] = {
      customConvertibilityRules.toVector.map(x => {
        (x._1._1, x._1._2, x._2)
      })
    }

    // DO A BFS
    var foundConversions: Map[NewMapTypeSystem.TypeId, Vector[FunctionWithMatchingRules]] = Map(typeId1 -> Vector.empty)

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
        val oldConversionRule: Vector[FunctionWithMatchingRules] = foundConversions.get(firstType).get
        val newConversionRule = oldConversionRule :+ applicableRule._3
        foundConversions = foundConversions + (foundType -> newConversionRule)
        unprocessedTypes = unprocessedTypes :+ foundType
      }

      unprocessedTypes = unprocessedTypes.drop(1)
      processedTypes = processedTypes :+ firstType
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
        "Long" -> java.util.UUID.randomUUID,
        "Double" -> java.util.UUID.randomUUID,
        "Uuit" -> java.util.UUID.randomUUID,

        "Type" -> java.util.UUID.randomUUID,
        "HistoricalType" -> java.util.UUID.randomUUID,
        "UndefinedType" -> java.util.UUID.randomUUID,
        "Identifier" -> java.util.UUID.randomUUID,

        "Map" -> java.util.UUID.randomUUID,
        "Struct" -> java.util.UUID.randomUUID,
        "Case" -> java.util.UUID.randomUUID,
        "Subtype" -> java.util.UUID.randomUUID
      )
    ),
  )

  val emptyStruct: NewMapType = StructT(UMap(Vector.empty), IndexT(UIndex(0)), RequireCompleteness, BasicMap)
}