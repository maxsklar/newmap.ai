package ai.newmap.model

import ai.newmap.interpreter.{CommandMaps, Evaluator, IterationUtils, RetrieveType, SubtypeUtils, TypeChecker}

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.ListMap
import ai.newmap.util.{Failure, Outcome, Success}
import java.util.UUID

// Additional things to keep track of: latest versions of all versioned objects??
case class Environment(
  // A History of the commands given to this environment
  commands: Vector[EnvironmentCommand] = Vector.empty,

  // A Map of all the variable bindings
  idToObject: ListMap[String, NewMapObject] = ListMap.empty,

  latestVersionNumber: Map[UUID, Long] = ListMap.empty,
  storedVersionedTypes: Map[VersionedObjectKey, NewMapObject] = ListMap.empty,

  // Also a stored versioned type, but a special one!
  typeSystem: NewMapTypeSystem = NewMapTypeSystem.initTypeSystem,
  channelIdToType: Map[String, NewMapType] = Environment.initialChannelToType,

  // Map representing a list of objects to send commands to from a channelId
  channelIdToObjectCommands: Map[String, Set[String]] = Map.empty,
  printStdout: Boolean = true
) {
  override def toString: String = {
    val builder: StringBuilder = new StringBuilder()

    for ((id, nObject) <- idToObject) {
      val command = FullEnvironmentCommand(id, nObject)
      builder.append(s"${command.toString}\n")
    }

    builder.toString
  }

  def lookupValue(identifier: String): Option[NewMapObject] = {
    idToObject.get(identifier)
  }

  case class LookupVersionedObjectReturnValue(
    key: VersionedObjectKey,
    nType: NewMapType
  )

  def lookupVersionedObject(
    id: String
  ): Outcome[LookupVersionedObjectReturnValue, String] = {
    for {
      versionedObject <- Outcome(lookupValue(id), s"Identifier $id not found!")

      versionedO <- versionedObject match {
        case NewMapObject(ULink(key), nType) => Success(LookupVersionedObjectReturnValue(key, nType))
        case NewMapObject(ParamId(_), _) => Failure(s"Identifier $id is a parameter, should be an object")
        case nObject => Failure(s"Identifier $id does not point to a versioned object. It is actually ${nObject}.")
      }
    } yield versionedO
  }

  def printTypes: String = {
    val builder: StringBuilder = new StringBuilder()
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

  def printChannels: String = {
    val builder: StringBuilder = new StringBuilder()
    builder.append(s"Current Channels\n")

    for {
      channels <- channelIdToType
      channelName = channels._1
      channelType = channels._2
    } {
      builder.append(s"${channelName}\t${channelType.displayString(this)}\n")
    }
    builder.toString
  }

  def print(): Unit = {
    println(this.toString())
  }

  def newCommand(command: EnvironmentCommand): Environment = {
    val newCommands = commands :+ command

    command match {
      case FullEnvironmentCommand(s, nObjectFunc, true) => {
        val uObject = nObjectFunc.uObject
        val nType = nObjectFunc.nType

        val updatedEnv = this.newCommand(
          ApplyIndividualCommand(
            "__FunctionSystem",
            UMap(
              Vector(UIndex(0) -> UIdentifier(s), UIndex(1) -> UCase(typeSystem.typeToUntaggedObject(nType), uObject))
            )
          )
        )

        val fSystemId = updatedEnv.lookupValue("__FunctionSystem") match {
          case Some(NewMapObject(ULink(VersionedObjectKey(_, uuid)), _)) => uuid
          case f => throw new Exception(s"Can't handle Function System $f")
        }

        val fLink = UFunctionLink(UIdentifier(s), fSystemId)

        updatedEnv.copy(
          commands = newCommands,
          idToObject = idToObject + (s -> NewMapObject(fLink, nType))
        )
      }
      case FullEnvironmentCommand(s, nObject, false) => {
        val nObjectO = for {
          evaluatedObject <- Evaluator(nObject.uObject, this)

          constantObject = Evaluator.stripVersioningU(evaluatedObject, this)
          nObject <- TypeChecker.tagAndNormalizeObject(constantObject, nObject.nType, this)
        } yield nObject

        nObjectO match {
          case Success(nObject) => {
            this.copy(
              commands = newCommands,
              idToObject = idToObject + (s -> nObject)
            )
          }
          case Failure(s) => throw new Exception("FullEnvironmentCommand: " + nObject.displayString(this))
        }


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
        val versionedObject = NewMapObject(ULink(key), nType)

        this.copy(
          commands = newCommands,
          idToObject = idToObject + (s -> versionedObject),
          latestVersionNumber = latestVersionNumber + (uuid -> 0L),
          storedVersionedTypes = storedVersionedTypes + (key -> NewMapObject(initValue, nType))
        )
      }
      case NewTypeCommand(s, nType) => {
        val uType = typeSystem.typeToUntaggedObject(nType)

        val parameterType = typeSystem.typeToUntaggedObject(NewMapTypeSystem.emptyStruct)
        val parameterPattern = UStruct(Vector.empty)

        val newTypeSystem = typeSystem.createNewCustomType(s, parameterType, parameterPattern, uType) match {
          case Success(s) => s
          case Failure(f) => throw new Exception(f)
        }

        this.copy(
          commands = newCommands,
          typeSystem = newTypeSystem
        )
      }
      case NewParamTypeCommand(s, paramList, nType) => {
        val parameterPattern = if (paramList.length == 1) {
          UWildcardPattern(paramList.head._1)
        } else {
          UStruct(paramList.map(param => UWildcardPattern(param._1)))
        }

        val paramT = if (paramList.length == 1) {
          paramList.head._2
        } else {
          StructT(
            UMap(paramList.zipWithIndex.map(x => UIndex(x._2) -> typeSystem.typeToUntaggedObject(x._1._2))),
            IndexT(UIndex(paramList.length))
          )
        }

        val uType = typeSystem.typeToUntaggedObject(nType)

        val newTypeSystem = typeSystem.createNewCustomType(s, typeSystem.typeToUntaggedObject(paramT), parameterPattern, uType) match {
          case Success(s) => s
          case Failure(f) => throw new Exception(f)
        }

        this.copy(
          commands = newCommands,
          typeSystem = newTypeSystem
        )
      }
      case NewTypeClassCommand(s, typeTransform) => {
        val nType = TypeClassT(typeTransform, UMap(Vector.empty))
        val uType = typeSystem.typeToUntaggedObject(nType)

        val parameterType = typeSystem.typeToUntaggedObject(NewMapTypeSystem.emptyStruct)
        val parameterPattern = UStruct(Vector.empty)

        val newTypeSystem = typeSystem.createNewCustomType(s, parameterType, parameterPattern, uType) match {
          case Success(s) => s
          case Failure(f) => throw new Exception(f)
        }

        this.copy(
          commands = newCommands,
          typeSystem = newTypeSystem
        )
      }
      case IterateIntoCommand(iterableObject, destinationObject) => {
        IterationUtils.iterateObject(iterableObject, this) match {
          case Success(commandList) => {
            var returnedEnv = this
            for {
              command <- commandList
            } {
              val convertedCommandO = for {
                versionedObjectLink <- lookupVersionedObject(destinationObject)
                nType = versionedObjectLink.nType

                itemType <- IterationUtils.iterationItemType(nType, this)
                commandObj = NewMapObject(command, itemType)
                
                inputT <- CommandMaps.getCommandInputOfCommandType(nType, this)
                convertedCommand <- SubtypeUtils.attemptConvertObjectToType(commandObj, inputT, this)
              } yield {
                convertedCommand.uObject
              }

              convertedCommandO match {
                case Failure(reason) => throw new Exception(reason)
                case Success(convertedCommand) => {
                  returnedEnv = returnedEnv.newCommand(ApplyIndividualCommand(destinationObject, convertedCommand))
                }
              }
            }

            returnedEnv
          }
          case Failure(reason) => {
            throw new Exception(s"Iterate into command failed: $reason")
          }
        }
      }
      case ApplyIndividualCommand(s, command) => {
        // This split on lookupVersionedObject suggests that we may want to refactor
        // Code is repeated!!

        // Maybe s is also a stdout (as in println)

        val retVal = lookupVersionedObject(s) match {
          case Success(versionLink) => {
            for {
              latestVersion <- Evaluator.latestVersion(versionLink.key.uuid, this)
              currentState <- Evaluator.currentState(versionLink.key.uuid, this)

              newValue <- CommandMaps.updateVersionedObject(currentState, command, this)
            } yield {
              val newUuid = versionLink.key.uuid
              val newVersion = latestVersion + 1
              val newKey = VersionedObjectKey(newVersion, newUuid)

              // TODO - during this, versions that are no longer in use can be destroyed
              val newStoredVTypes = storedVersionedTypes + (newKey -> newValue)

              val newObject = NewMapObject(ULink(newKey), newValue.nType)

              this.copy(
                commands = newCommands,
                idToObject = idToObject + (s -> newObject),
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
            } yield {
              this.copy(
                commands = newCommands,
                typeSystem = newTypeSystem
              )
            }
          }
        }

        retVal match {
          case Success(s) => s
          case Failure(f) => throw new Exception(s"type checker failed on command $command\n$f")
        }
      }
      case ForkEnvironmentCommand(s, key) => {
        val uuid = java.util.UUID.randomUUID
        val version = Evaluator.latestVersion(key.uuid, this).toOption.get
        val current = Evaluator.currentState(key.uuid, this).toOption.get

        val newKey = VersionedObjectKey(version, uuid)
        val versionedObject = NewMapObject(ULink(newKey), current.nType)

        this.copy(
          commands = newCommands,
          idToObject = idToObject + (s -> versionedObject),
          latestVersionNumber = latestVersionNumber + (uuid -> version),
          storedVersionedTypes = storedVersionedTypes + (newKey -> current)
        )
      }
      case ExpOnlyEnvironmentCommand(nObject) => {
        val uType = typeSystem.typeToUntaggedObject(nObject.nType)
        this.newCommand(ApplyIndividualCommand("res", UCase(uType, nObject.uObject)))
      }
      case AddChannel(channel, nType) => {
        val channelName = channel match {
          case UIdentifier(s) => s
          case _ => throw new Exception(s"illegal channel: $channel")
        }

        println(s"Add channel $channelName -- ${nType.displayString(this)}")

        // TODO: Check that the channel doesn't already exist!
        // (and if it does - remove it?)

        this.copy(
          channelIdToType = this.channelIdToType + (channelName -> nType)
        )
      }
      case ConnectChannel(channel, versionedObject) => {
        val channelName = channel match {
          case UIdentifier(s) => s
          case _ => throw new Exception(s"illegal channel: $channel")
        }

        val currentConnections = this.channelIdToObjectCommands.get(channelName).getOrElse(Set.empty)

        this.copy(
          channelIdToObjectCommands = this.channelIdToObjectCommands + (channelName -> (currentConnections + versionedObject))
        )
      }
      case DisconnectChannel(channel, versionedObject) => {
        val channelName = channel match {
          case UIdentifier(s) => s
          case _ => throw new Exception(s"illegal channel: $channel")
        }

        val currentConnections = this.channelIdToObjectCommands.get(channelName).getOrElse(Set.empty)
        
        println(s"Disconnecting channel $channelName -- $versionedObject")
        this.copy(
          channelIdToObjectCommands = this.channelIdToObjectCommands + (channelName -> (currentConnections - versionedObject))
        )
      }
      case OutputToChannel(nObject, channel) => {
        val channelName = channel match {
          case UIdentifier(s) => s
          case _ => throw new Exception(s"illegal channel: $channel")
        }

        val channelType: NewMapType = channelIdToType.get(channelName).getOrElse(UndefinedT)

        var returnedEnv = this
        val currentConnections = this.channelIdToObjectCommands.get(channelName).getOrElse(Set.empty)

        val taggedObject = NewMapObject(nObject, channelType)

        if (channelName == "stdout" && printStdout) {
          returnedEnv = returnedEnv.newCommand(OutputToStdout(nObject))
        }

        for {
          connection <- currentConnections
        } {
          returnedEnv = returnedEnv.newCommand(IterateIntoCommand(taggedObject, connection))
        }

        returnedEnv
      }
      case OutputToStdout(nObject) => {
        val taggedObject = NewMapObject(nObject, CustomT("String", UStruct(Vector.empty)))

        // TODO: obviously this can be way more efficient!
        for {
          chars <- IterationUtils.iterateObject(taggedObject, this)
        } yield {
          val listOfChars = chars.flatMap(_ match {
            case UCharacter(c) => Some(c)
            case _ => None
          })

          System.out.print(listOfChars.mkString(""))
        }

        this
      }
      case IterateIntoChannel(nObject, channel) => {
        // TODO - this is repeated code!
        // Combine channels and objects, and this will be unified
        val channelName = channel match {
          case UIdentifier(s) => s
          case _ => throw new Exception(s"illegal channel: $channel")
        }

        val channelType: NewMapType = channelIdToType.get(channelName).getOrElse(UndefinedT)

        val taggedObject = NewMapObject(nObject, channelType)

        IterationUtils.iterateObject(taggedObject, this) match {
          case Success(commandList) => {
            var returnedEnv = this
            for {
              command <- commandList
            } {
              returnedEnv = returnedEnv.newCommand(OutputToChannel(command, channel))
            }

            returnedEnv
          }
          case Failure(reason) => {
            throw new Exception(s"Iterate into command failed: $reason")
          }
        }
      }
      case EmptyEnvironmentCommand => this
    }
  }

  def newCommands(newCommands: Vector[EnvironmentCommand]): Environment = {
    var env = this
    for (com <- newCommands) {
      env = env.newCommand(com)
    }
    env
  }

  def typeAsObject(nType: NewMapType): NewMapObject = {
    val uType = typeSystem.typeToUntaggedObject(nType)
    NewMapObject(uType, TypeT)
  }

  def toTypeTransform(
    inputT: NewMapType,
    outputT: NewMapType
  ): UntaggedObject = {
    UMapPattern(typeSystem.typeToUntaggedObject(inputT), typeSystem.typeToUntaggedObject(outputT))
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

    StructT(UMap(paramsToObject), IdentifierT)
  }

  def caseTypeFromParams(params: Vector[(String, NewMapType)]) = {
    val paramsToObject = {
      params.map(x => UIdentifier(x._1) -> Base.typeSystem.typeToUntaggedObject(x._2))
    }

    CaseT(UMap(paramsToObject), IdentifierT)
  }

  // For Debugging
  def printEnvWithoutBase(env: Environment): Unit = {
    for ((id, nObject) <- env.idToObject) {
      if (!Base.idToObject.contains(id)) {
        val command = FullEnvironmentCommand(id, nObject)
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
      UMap(inputs.zipWithIndex.map(x => UIndex(x._2) -> Base.typeSystem.typeToUntaggedObject(x._1._2))),
      IndexT(UIndex(inputs.length))
    )

    val structP = UStruct(inputs.map(x => UWildcardPattern(x._1)))

    NewMapObject(
      UMap(Vector(structP -> expression)),
      MapT(env.toTypeTransform(structT, TypeT), MapConfig(RequireCompleteness, SimpleFunction))
    )
  }

  // TODO - can we say that these are already in the type name space, so we can remove these?
  def typeAsObject(nType: NewMapType): NewMapObject = {
    Base.typeAsObject(nType)
  }

  def typeAsUntaggedObject(nType: NewMapType): UntaggedObject = {
    Base.typeSystem.typeToUntaggedObject(nType)
  }

  // TODO - eventually make this empty and add it elsewhere!!
  val initialChannelToType = Map(
    "stdout" -> CustomT("String", UStruct(Vector.empty))
  )

  var Base: Environment = Environment()

  def buildSimpleMapT(typeTransform: UntaggedObject): UntaggedObject = {
    val config = MapConfig(RequireCompleteness, SimpleFunction)
    val nType = MapT(typeTransform, config)
    typeAsUntaggedObject(nType)
  }

  def buildSubtypeT(isMember: UntaggedObject, parentType: UntaggedObject, env: Environment): UntaggedObject = {
    UCase(UIdentifier("Subtype"), UStruct(Vector(
      isMember,
      parentType,
      UIdentifier("BasicMap")
    )))
  }

  def buildMapCreator(config: MapConfig, allowGenerics: Boolean): NewMapObject = {
    // Probably 
    val transformOfTransformConfig = MapConfig(
      CommandOutput,
      if (allowGenerics) SimpleFunction else BasicMap
    )

    val transformMapT = Base.typeSystem.typeToUntaggedObject(MapT(ParamId("typeTransform"), config))

    NewMapObject(
      UMap(Vector(UWildcardPattern("typeTransform") -> transformMapT)),
      MapT(
        Base.toTypeTransform(
          MapT(Base.toTypeTransform(TypeT, TypeT), transformOfTransformConfig),
          TypeT
        ),
        config
      )
    )
  }

  Base = Base.newCommands(Vector(
    eCommand("Type", typeAsObject(TypeT)),
    eCommand("Count", typeAsObject(CountT)),
    eCommand("Char", typeAsObject(CharacterT)),
    eCommand("Identifier", typeAsObject(IdentifierT)),
    eCommand("Increment", NewMapObject(
      UMap(Vector(UWildcardPattern("i") -> UCase(UIdentifier("Inc"), ParamId("i")))),
      MapT(Base.toTypeTransform(CountT, CountT), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("IsCommand", NewMapObject(
      IsCommandFunc,
      MapT(Base.toTypeTransform(TypeT, BooleanT), MapConfig(CommandOutput, SimpleFunction))
    )),
    eCommand("Boolean", typeAsObject(BooleanT)),
    eCommand("Sequence", NewMapObject(
      UMap(Vector(UWildcardPattern("key") -> buildSimpleMapT(UMap(Vector(UIndex(0) -> ParamId("key")))))),
      MapT(Base.toTypeTransform(TypeT, TypeT), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("Map", buildMapCreator(MapConfig(CommandOutput, BasicMap), false)),
    eCommand("GenericMap", buildMapCreator(MapConfig(RequireCompleteness, SimpleFunction), true)),
    eCommand("ReqMap", buildMapCreator(MapConfig(RequireCompleteness, SimpleFunction), false)),
    eCommand("Table", buildMapCreator(MapConfig(RequireCompleteness, SimpleFunction), false)),
    eCommand("CaseType", typeAsObject(CaseT(UMap(Vector.empty), IdentifierT, BasicMap))),
    eCommand("StructSeq", typeAsObject(StructT(UMap(Vector.empty), IndexT(UIndex(0))))),
    eCommand("Subtype", NewMapObject(
      UMap(Vector(UWildcardPattern("t") -> buildSubtypeT(UMap(Vector.empty), ParamId("t"), Base))),
      MapT(Base.toTypeTransform(TypeT, TypeT), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    NewTypeClassCommand("_default", UMapPattern(UWildcardPattern("t"), ParamId("t"))),
    NewTypeClassCommand("_typeOf", 
      UMapPattern(
        UWildcardPattern("t"),
        buildSimpleMapT(
          UMap(Vector(UWildcardPattern("_") -> typeAsUntaggedObject(TypeT))),
        )
      )
    ),
    ApplyIndividualCommand(
      "_typeOf",
      UMap(Vector(UWildcardPattern("t") -> UMap(Vector(UWildcardPattern("_") -> ParamId("t")))))
    ),
    NewParamTypeCommand(
      "Array",
      Vector("T" -> TypeT),
      CaseT(
        UMap(Vector(UWildcardPattern("i") -> typeAsUntaggedObject(
          MapT(
            Base.toTypeTransform(IndexT(ParamId("i")), ParamIdT("T")),
            MapConfig(RequireCompleteness, SimpleFunction)
          )
        ))),
        CountT,
        SimpleFunction
      )
    ),
    NewTypeCommand("Object", NewMapO.taggedObjectT),
    NewVersionedStatementCommand("__FunctionSystem", FunctionalSystemT(Vector.empty)),
    eCommand("+", NewMapObject(
      UPlus,
      MapT(Base.toTypeTransform(
        MapT(Base.toTypeTransform(IndexT(UIndex(2)), CountT), MapConfig(RequireCompleteness, BasicMap)),
        CountT
      ), MapConfig(RequireCompleteness, SimpleFunction))
    ))
  ))
}