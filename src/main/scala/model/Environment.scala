package ai.newmap.model

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.ListMap
import ai.newmap.interpreter._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

sealed abstract class EnvironmentCommand

case class FullEnvironmentCommand(
  id: String,
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = {
    s"val $id = ${nObject}"
  }
}

case class NewVersionedStatementCommand(
  id: String,
  nType: NewMapType
) extends EnvironmentCommand {
  override def toString: String = {
    s"ver $id = new ${nType}"
  }
}

case class NewTypeCommand(
  id: String,
  nType: NewMapType
) extends EnvironmentCommand {
  override def toString: String = {
    s"data $id = ${nType}"
  }
}

case class NewParamTypeCommand(
  id: String,
  paramList: Vector[(String, NewMapType)],
  nType: NewMapType
) extends EnvironmentCommand {
  override def toString: String = {
    s"data $id ${paramList}"
  }
}

case class ApplyIndividualCommand(
  id: String,
  nObject: UntaggedObject
) extends EnvironmentCommand {
  override def toString: String = {
    "" //s"update $id $nObject"
  }
}

case class ForkEnvironmentCommand(
  id: String,
  vObject: VersionedObjectLink
) extends EnvironmentCommand {
  override def toString: String = {
    s"ver $id = fork ${vObject}"
  }
}

case class ParameterEnvironmentCommand(
  id: String,
  nType: NewMapType
) extends EnvironmentCommand {
  override def toString: String = {
    s"parameter $id: ${nType}"
  }
}

case class ExpOnlyEnvironmentCommand(
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = nObject.toString
}

sealed abstract class EnvironmentValue

case class EnvironmentBinding(nObject: NewMapObject) extends EnvironmentValue
case class EnvironmentParameter(nType: NewMapType) extends EnvironmentValue


// Additional things to keep track of: latest versions of all versioned objects??
case class Environment(
  commands: Vector[EnvironmentCommand] = Vector.empty,
  idToObject: ListMap[String, EnvironmentValue] = ListMap.empty,

  latestVersionNumber: Map[UUID, Long] = ListMap.empty,
  storedVersionedTypes: Map[VersionedObjectKey, NewMapObject] = ListMap.empty,
  typeSystem: NewMapTypeSystem = NewMapTypeSystem.initTypeSystem // Also a stored versioned type, but a special one!
) {
  def lookup(identifier: String): Option[EnvironmentValue] = {
    idToObject.get(identifier)
  }

  override def toString: String = {
    val builder: StringBuilder = new StringBuilder()
    for ((id, envValue) <- idToObject) {
      val command = envValue match {
        case EnvironmentBinding(nObject) => FullEnvironmentCommand(id, nObject)
        case EnvironmentParameter(nTypeClass) => ParameterEnvironmentCommand(id, nTypeClass)
      }
      builder.append(s"${command.toString}\n")
    }
    builder.toString
  }

  def printTypes: String = {
    val builder: StringBuilder = new StringBuilder()
    builder.append(s"Current State: ${typeSystem.currentState}\n")

    for {
      typeMap <- typeSystem.currentMapping.toVector
      typeName = typeMap._1
      typeId = typeMap._2

      parameterTypeOpt = typeSystem.typeToParameterType.get(typeId)

      parameterTypeString = parameterTypeOpt.map(x => "\t" + x.toString).getOrElse("")
    } {
      builder.append(s"${typeMap._1}${parameterTypeString}\n")
    }
    builder.toString
  }

  def print(): Unit = {
    println(this.toString())
  }

  def newCommand(command: EnvironmentCommand): Environment = {
    val newCommands = commands :+ command

    command match {
      case FullEnvironmentCommand(s, nObject) => {
        this.copy(
          commands = newCommands,
          idToObject = idToObject + (s -> EnvironmentBinding(nObject))
        )
      }
      case NewVersionedStatementCommand(s, nType) => {
        val uuid = java.util.UUID.randomUUID

        val defaultOutcome = CommandMaps.getDefaultValueOfCommandType(
          nType,
          this
        )

        defaultOutcome match {
          case Success(_) => ()
          case Failure(f) => throw new Exception(f)
        }

        val initValue = defaultOutcome.toOption.get
        val key = VersionedObjectKey(0L, uuid)
        val versionedObject = VersionedObjectLink(key)
        val envValue = EnvironmentBinding(versionedObject)

        this.copy(
          commands = newCommands,
          idToObject = idToObject + (s -> envValue),
          latestVersionNumber = latestVersionNumber + (uuid -> 0L),
          storedVersionedTypes = storedVersionedTypes + (key -> TaggedObject(initValue, nType))
        )
      }
      case NewTypeCommand(s, nType) => {
        //val uuid = java.util.UUID.randomUUID
        //val key = VersionedObjectKey(0L, uuid)
        //val versionedObject = VersionedObjectLink(key)
        //val envValue = EnvironmentBinding(versionedObject)

        val uType = typeSystem.typeToUntaggedObject(nType)

        //val typeAsObject = TaggedObject(uType, HistoricalTypeT(typeSystem.currentState))

        //val envValue = EnvironmentBinding(typeAsObject)

        val parameterType = typeSystem.typeToUntaggedObject(NewMapTypeSystem.emptyStruct)
        val parameterPattern = UStruct(Vector.empty)

        val newTypeSystem = typeSystem.createNewCustomType(s, parameterType, parameterPattern, uType) match {
          case Success(s) => s
          case Failure(f) => throw new Exception(f)
        }
        

        this.copy(
          commands = newCommands,
          //idToObject = idToObject + (s -> envValue),
          //latestVersionNumber = latestVersionNumber + (uuid -> 0L),
          //storedVersionedTypes = storedVersionedTypes + (key -> TaggedObject(typeSystem.typeToUntaggedObject(CustomT(uuid, nType)), TypeT))
          typeSystem = newTypeSystem
        )
      }
      case NewParamTypeCommand(s, paramList, nType) => {
        val parameterPattern = UStruct(paramList.map(param => UWildcardPattern(param._1)))
        val paramT = StructT(
          paramList.zipWithIndex.map(x => UIndex(x._2) -> typeSystem.typeToUntaggedObject(x._1._2)),
          IndexT(paramList.length)
        )

        val uType = typeSystem.typeToUntaggedObject(nType)

        val newTypeSystem = typeSystem.createNewCustomType(s, typeSystem.typeToUntaggedObject(paramT), parameterPattern, uType) match {
          case Success(s) => s
          case Failure(f) => throw new Exception(f)
        }

        this.copy(
          commands = newCommands,
          //idToObject = idToObject + (id -> envValue),
          typeSystem = newTypeSystem
        )
      }
      case ApplyIndividualCommand(s, command) => {
        // This split on lookupVersionedObject suggests that we may want to refactor
        // Code is repeated!!

        val retVal = Evaluator.lookupVersionedObject(s, this) match {
          case Success(versionLink) => {
            for {
              latestVersion <- Evaluator.latestVersion(versionLink.key.uuid, this)
              currentState <- Evaluator.currentState(versionLink.key.uuid, this)
              nType = RetrieveType.fromNewMapObject(currentState, this)
              newValue <- CommandMaps.updateVersionedObject(currentState, command, this)
            } yield {
              val newUuid = versionLink.key.uuid
              val newVersion = latestVersion + 1
              val newKey = VersionedObjectKey(newVersion, newUuid)

              // TODO - during this, versions that are no longer in use can be destroyed
              val newStoredVTypes = storedVersionedTypes + (newKey -> newValue)

              this.copy(
                commands = newCommands,
                idToObject = idToObject + (s -> EnvironmentBinding(VersionedObjectLink(newKey))),
                latestVersionNumber = latestVersionNumber + (newUuid -> newVersion),
                storedVersionedTypes = newStoredVTypes
              )
            }
          }
          case Failure(objectLookupFailureMessage) => {
            val currentState = typeSystem.currentState

            for {
              latestNamespace <- Outcome(typeSystem.historicalMapping.get(currentState), s"Type System missing latest namespace $currentState")
              typeId <- Outcome(latestNamespace.get(s), s"Couldn't apply command to value $s. Not found in object or type namespace. Object space failure: $objectLookupFailureMessage")
              currentUnderlyingType <- Outcome(typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find underlying type for $s")

              currentParameterPattern = currentUnderlyingType._1
              currentUnderlyingExp = currentUnderlyingType._2

              underlyingT <- typeSystem.convertToNewMapType(currentUnderlyingExp)

              response <- CommandMaps.expandType(underlyingT, command, this)

              newUnderlyingType = typeSystem.typeToUntaggedObject(response.newType)

              newTypeSystem <- typeSystem.upgradeCustomType(s, newUnderlyingType, response.converter)

              //typeAsObject = TaggedObject(newUnderlyingType, HistoricalTypeT(newTypeSystem.currentState))
              //envValue = EnvironmentBinding(typeAsObject)
            } yield {
              this.copy(
                commands = newCommands,
                //idToObject = idToObject + (s -> envValue),
                typeSystem = newTypeSystem
              )
            }
          }
        }

        retVal match {
          case Success(s) => s
          case Failure(f) => throw new Exception(s"type checker failed on command $command: $f")
        }
      }
      case ForkEnvironmentCommand(s, vObject) => {
        val uuid = java.util.UUID.randomUUID
        val version = Evaluator.latestVersion(vObject.key.uuid, this).toOption.get
        val current = Evaluator.currentState(vObject.key.uuid, this).toOption.get
        val key = VersionedObjectKey(version, uuid)
        val versionedObject = VersionedObjectLink(key)
        val envValue = EnvironmentBinding(versionedObject)

        this.copy(
          commands = newCommands,
          idToObject = idToObject + (s -> envValue),
          latestVersionNumber = latestVersionNumber + (uuid -> version),
          storedVersionedTypes = storedVersionedTypes + (key -> current)
        )
      }
      case ParameterEnvironmentCommand(s, nTypeClass) => {
        this.copy(
          commands = newCommands,
          idToObject = idToObject + (s -> EnvironmentParameter(nTypeClass))
        )
      }
      case ExpOnlyEnvironmentCommand(nObject) => {
        // TODO: save this in the result list
        this
      }
    }
  }

  def newCommands(newCommands: Vector[EnvironmentCommand]): Environment = {
    var env = this
    for (com <- newCommands) {
      env = env.newCommand(com)
    }
    env
  }

  def newParam(id: String, nType: NewMapType): Environment = {
    newCommand(ParameterEnvironmentCommand(id, nType))
  }

  def newParams(xs: Vector[(String, NewMapType)]) = {
    newCommands(xs.map(x => ParameterEnvironmentCommand(x._1, x._2)))
  }

  def typeAsObject(nType: NewMapType): NewMapObject = {
    val uType = typeSystem.typeToUntaggedObject(nType) 
    TaggedObject(uType, TypeT)
  }

  def toTypeTransform(
    inputT: NewMapType,
    outputT: NewMapType
  ): UntaggedObject = {
    UMap(Vector(typeSystem.typeToUntaggedObject(inputT) -> typeSystem.typeToUntaggedObject(outputT)))
  }
}

object Environment {
  def eCommand(id: String, nObject: NewMapObject): EnvironmentCommand = {
    FullEnvironmentCommand(id, nObject)
  }



  def fullFuncT(typeTransform: UntaggedObject): NewMapType = {
    MapT(typeTransform, MapConfig(RequireCompleteness, FullFunction))
  }

  def structTypeFromParams(params: Vector[(String, NewMapType)]) = {
    val paramsToObject = {
      params.map(x => UIdentifier(x._1) -> Base.typeSystem.typeToUntaggedObject(x._2))
    }

    StructT(paramsToObject, IdentifierT)
  }

  def caseTypeFromParams(params: Vector[(String, NewMapType)]) = {
    val paramsToObject = {
      params.map(x => UIdentifier(x._1) -> Base.typeSystem.typeToUntaggedObject(x._2))
    }

    CaseT(paramsToObject, IdentifierT)
  }

  // For Debugging
  def printEnvWithoutBase(env: Environment): Unit = {
    for ((id, envValue) <- env.idToObject) {
      if (!Base.idToObject.contains(id)) {
        val command = envValue match {
          case EnvironmentBinding(nObject) => FullEnvironmentCommand(id, nObject)
          case EnvironmentParameter(nType) => ParameterEnvironmentCommand(id, nType)
        }
        println(command.toString)
      }
    }
  }

  // Somewhat complex for now, but this is how a pattern/function definition is built up!
  // In code, this should be done somewhat automatically
  def buildDefinitionWithParameters(
    inputs: Vector[(String, NewMapType)], // A map from parameters and their type
    expression: UntaggedObject,
    env: Environment
  ): NewMapObject = {
    val structT = StructT(
      inputs.zipWithIndex.map(x => UIndex(x._2) -> Base.typeSystem.typeToUntaggedObject(x._1._2)),
      IndexT(inputs.length)
    )

    val structP = UStruct(inputs.map(x => UWildcardPattern(x._1)))

    TaggedObject(
      UMap(Vector(structP -> expression)),
      MapT(env.toTypeTransform(structT, TypeT), MapConfig(RequireCompleteness, SimpleFunction))
    )
  }

  // TODO - can we say that these are already in the type name space, so we can remove these?
  def typeAsObject(nType: NewMapType): NewMapObject = {
    Environment().typeAsObject(nType)
  }

  def typeAsUntaggedObject(nType: NewMapType): UntaggedObject = {
    Environment().typeSystem.typeToUntaggedObject(nType)
  }

  var Base: Environment = Environment()

  def buildTableT(keyType: UntaggedObject, valueType: UntaggedObject, env: Environment): UntaggedObject = {
    buildSimpleMapT(keyType, valueType, MapConfig(RequireCompleteness, SimpleFunction), env)
  }

  def buildSimpleMapT(keyType: UntaggedObject, valueType: UntaggedObject, config: MapConfig, env: Environment): UntaggedObject = {
    val typeTransform = UMap(Vector(keyType -> valueType))
    val nType = MapT(typeTransform,  config)
    typeAsUntaggedObject(nType)
  }

  def buildSubtypeT(isMember: UntaggedObject, parentType: UntaggedObject, env: Environment): UntaggedObject = {
    UCase(UIdentifier("Subtype"), UStruct(Vector(
      isMember,
      parentType,
      UIdentifier("BasicMap")
    )))
  }

  def buildMapT(typeTransform: UntaggedObject, config: MapConfig, env: Environment): UntaggedObject = {
    env.typeSystem.typeToUntaggedObject(MapT(typeTransform, config))
  }

  Base = Base.newCommands(Vector(
    //eCommand("Any", typeAsObject(AnyT)),
    eCommand("Type", typeAsObject(TypeT)),
    eCommand("Count", typeAsObject(CountT)),
    eCommand("Identifier", typeAsObject(IdentifierT)),
    eCommand("Increment", TaggedObject(
      UMap(Vector(UWildcardPattern("i") -> UCase(UIdentifier("Inc"), ParamId("i")))),
      MapT(Base.toTypeTransform(CountT, CountT), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("IsCommand", TaggedObject(
      IsCommandFunc,
      MapT(Base.toTypeTransform(TypeT, IndexT(2)), MapConfig(CommandOutput, SimpleFunction))
    )),
    eCommand("Boolean", typeAsObject(BooleanT)),
    eCommand("Sequence", TaggedObject(
      UMap(Vector(UWildcardPattern("key") -> buildTableT(UIndex(0), ParamId("key"), Base))),
      MapT(Base.toTypeTransform(TypeT, TypeT), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("Map", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> NewMapO.commandT),
      buildSimpleMapT(ParamId("key"), ParamId("value"), MapConfig(CommandOutput, BasicMap), Base),
      Base
    )),
  eCommand("GenericMap", TaggedObject(
      UMap(Vector(UWildcardPattern("typeTransform") -> buildMapT(ParamId("typeTransform"), MapConfig(RequireCompleteness, SimpleFunction), Base))),
      MapT(
        Base.toTypeTransform(
          MapT(Base.toTypeTransform(TypeT, TypeT), MapConfig(CommandOutput, SimpleFunction)),
          TypeT
        ),
        MapConfig(RequireCompleteness, SimpleFunction)
      )
    )),
    eCommand("ReqMap", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> TypeT),
      buildSimpleMapT(ParamId("key"), ParamId("value"), MapConfig(RequireCompleteness, SimpleFunction), Base),
      Base
    )),
    eCommand("Table", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> TypeT),
      buildTableT(ParamId("key"), ParamId("value"), Base),
      Base
    )),
    eCommand("CaseType", typeAsObject(CaseT(Vector.empty, IdentifierT, BasicMap))),
    eCommand("Subtype", TaggedObject(
      UMap(Vector(UWildcardPattern("t") -> buildSubtypeT(UMap(Vector.empty), ParamId("t"), Base))),
      MapT(Base.toTypeTransform(TypeT, TypeT), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    NewVersionedStatementCommand("_default", TypeClassT(Vector(UWildcardPattern("t") -> ParamId("t")), Vector.empty)),
    NewVersionedStatementCommand("_typeOf", 
      TypeClassT(
        Vector(UWildcardPattern("t") -> buildSimpleMapT(ParamId("t"), typeAsUntaggedObject(TypeT), MapConfig(RequireCompleteness, SimpleFunction), Base)),
        Vector.empty
      )
    ),
    ApplyIndividualCommand("_typeOf", UCase(UWildcardPattern("t"), UMap(Vector(UWildcardPattern("_") -> ParamId("t"))))),
    NewParamTypeCommand(
      "Array",
      Vector("T" -> TypeT),
      CaseT(
        Vector(UWildcardPattern("i") -> typeAsUntaggedObject(MapT(Base.toTypeTransform(ParamIdT("i"), ParamIdT("T")), MapConfig(RequireCompleteness, SimpleFunction)))),
        CountT,
        SimpleFunction
      )
    )
  ))
}