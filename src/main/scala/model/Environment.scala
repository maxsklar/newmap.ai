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
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = {
    s"data $id = ${nObject}"
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
  nType: NewMapPattern // Pattern representing a type
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
case class EnvironmentParameter(nTypeClass: NewMapPattern) extends EnvironmentValue

// Additional things to keep track of: latest versions of all versioned objects??
case class Environment(
  commands: Vector[EnvironmentCommand] = Vector.empty,
  idToObject: ListMap[String, EnvironmentValue] = ListMap.empty,

  latestVersionNumber: Map[UUID, Long] = ListMap.empty,
  storedVersionedTypes: Map[VersionedObjectKey, NewMapObject] = ListMap.empty

  // TODO - add use-defined types as a special case of latest version number?
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
      builder.append(command.toString)
      builder.append("\n")
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

        val defaultOutcome = CommandMaps.getDefaultValueOfCommandType(UType(nType), this)

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
        val uuid = java.util.UUID.randomUUID
        val key = VersionedObjectKey(0L, uuid)
        val versionedObject = VersionedObjectLink(key)
        val envValue = EnvironmentBinding(versionedObject)

        this.copy(
          commands = newCommands,
          idToObject = idToObject + (s -> envValue),
          latestVersionNumber = latestVersionNumber + (uuid -> 0L),
          storedVersionedTypes = storedVersionedTypes + (key -> TaggedObject(UType(CustomT(uuid, nType)), TypeT))
        )
      }
      case NewParamTypeCommand(id, nObject) => {
        val uuid = java.util.UUID.randomUUID
        val key = VersionedObjectKey(0L, uuid)
        val versionedObject = VersionedObjectLink(key)
        val envValue = EnvironmentBinding(versionedObject)

        this.copy(
          commands = newCommands,
          idToObject = idToObject + (id -> envValue),
          latestVersionNumber = latestVersionNumber + (uuid -> 0L),
          storedVersionedTypes = storedVersionedTypes + (key -> nObject)
        )
      }
      case ApplyIndividualCommand(s, command) => {
        val retVal = for {
          versionLink <- Evaluator.lookupVersionedObject(s, this)
          latestVersion <- Evaluator.latestVersion(versionLink.key.uuid, this)
          currentState <- Evaluator.currentState(versionLink.key.uuid, this)
          nType = RetrieveType.fromNewMapObject(currentState, this)
          newValue <- if (nType == TypeT) {
            for {  
              currentUntagged <- Evaluator.removeTypeTag(currentState)
              currentAsType <- Evaluator.asType(currentUntagged, this)
              response <- CommandMaps.expandType(currentAsType, command, this)
              // TODO - we must contend with expandedKeyResponse.converter
              // - This will allow us to convert objects from one type to another - needs to be stored in the environment!
            } yield TaggedObject(UType(response.newType), TypeT)
          } else {
            currentState match {
              case TaggedObject(UParametrizedCaseT(parameters, _), _) => {
                CommandMaps.expandParametrizedCaseType(currentState, command, this.newParams(parameters))
              }
              case _ => CommandMaps.updateVersionedObject(currentState, command, this)
            }
          }
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

  // TODO - should we ensure that nType is actually a type?
  def newParam(id: String, nType: NewMapType): Environment = {
    newCommand(ParameterEnvironmentCommand(id, ObjectPattern(UType(nType))))
  }

  def newParamTypeClass(id: String, nTypeClass: NewMapPattern): Environment = {
    newCommand(ParameterEnvironmentCommand(id, nTypeClass))
  }

  def newParams(xs: Vector[(String, NewMapType)]) = {
    newCommands(xs.map(x => ParameterEnvironmentCommand(x._1, ObjectPattern(UType(x._2)))))
  }
}

object Environment {
  def eCommand(id: String, nObject: NewMapObject): EnvironmentCommand = {
    FullEnvironmentCommand(id, nObject)
  }

  def simpleFuncT(inputType: NewMapType, outputType: NewMapType): NewMapType = {
    MapT(inputType, outputType, MapConfig(RequireCompleteness, BasicMap))
  }

  def fullFuncT(inputType: NewMapType, outputType: NewMapType): NewMapType = {
    MapT(inputType, outputType, MapConfig(RequireCompleteness, FullFunction))
  }

  def structTypeFromParams(params: Vector[(String, NewMapType)]) = {
    val paramsToObject = {
      params.map(x => ObjectPattern(UIdentifier(x._1)) -> ObjectExpression(UType(x._2)))
    }

    StructT(paramsToObject, IdentifierT)
  }

  def caseTypeFromParams(params: Vector[(String, NewMapType)]) = {
    val paramsToObject = {
      params.map(x => ObjectPattern(UIdentifier(x._1)) -> ObjectExpression(UType(x._2)))
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
    expression: NewMapExpression
  ): NewMapObject = {
    val structT = StructT(
      inputs.zipWithIndex.map(x => ObjectPattern(UIndex(x._2)) -> ObjectExpression(UType(x._1._2))),
      IndexT(inputs.length)
    )

    val structP = StructPattern(inputs.map(x => WildcardPattern(x._1)))

    TaggedObject(
      UMap(Vector(structP -> expression)),
      MapT(structT, TypeT, MapConfig(RequireCompleteness, SimpleFunction))
    )
  }

  def typeAsObject(nType: NewMapType): NewMapObject = TaggedObject(UType(nType), TypeT)

  var Base: Environment = Environment().newCommands(Vector(
    //eCommand("Any", typeAsObject(AnyT)),
    eCommand("Type", typeAsObject(TypeT)),
    eCommand("Count", typeAsObject(CountT)),
    eCommand("Identifier", typeAsObject(IdentifierT)),
    eCommand("Increment", TaggedObject(IncrementFunc, MapT(CountT, CountT, MapConfig(RequireCompleteness, SimpleFunction)))),
    eCommand("IsCommand", TaggedObject(
      IsCommandFunc,
      MapT(TypeT, IndexT(2), MapConfig(CommandOutput, SimpleFunction))
    )),
    eCommand("Boolean", typeAsObject(BooleanT)),
    eCommand("Sequence", TaggedObject(
      UMap(Vector(WildcardPattern("key") -> BuildTableT(ObjectExpression(UIndex(0)), ParamId("key")))),
      MapT(TypeT, TypeT, MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("Map", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> NewMapO.commandT),
      BuildMapT(ParamId("key"), ParamId("value"), MapConfig(CommandOutput, BasicMap))
    )),
    eCommand("GenericMap", TaggedObject(
      UMap(Vector(WildcardPattern("typeTransform") -> BuildGenericMapT(ParamId("typeTransform"), MapConfig(RequireCompleteness, SimpleFunction)))),
      MapT(
        MapT(TypeT, TypeT, MapConfig(CommandOutput, SimpleFunction)),
        TypeT,
        MapConfig(RequireCompleteness, SimpleFunction)
      )
    )),
    eCommand("ReqMap", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> TypeT),
      BuildMapT(ParamId("key"), ParamId("value"), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("Table", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> TypeT),
      BuildTableT(ParamId("key"), ParamId("value"))
    )),
    eCommand("CaseType", TaggedObject(UType(CaseT(Vector.empty, IdentifierT, BasicMap)), TypeT)),
    eCommand("ParametrizedCaseType", TaggedObject(
      UParametrizedCaseT(Vector.empty, CaseT(Vector.empty, IdentifierT)),
      MapT(
        NewMapO.emptyStruct,
        TypeT,
        MapConfig(RequireCompleteness, SimpleFunction)
      )
    )),
    eCommand("Subtype", TaggedObject(
      UMap(Vector(WildcardPattern("t") -> BuildSubtypeT(ObjectExpression(UMap(Vector.empty)), ParamId("t")))),
      MapT(TypeT, TypeT, MapConfig(RequireCompleteness, SimpleFunction))
    )),
    NewVersionedStatementCommand("_default", TypeClassT(Vector(WildcardPattern("t") -> ParamId("t")), Vector.empty)),
  ))
}