package ai.newmap.model

import ai.newmap.interpreter.{UpdateCommandCalculator, Evaluator, IterationUtils, TypeConverter, TypeChecker, TypeExpander}

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

  // This is a (pattern) mapping from TypeT to a map from the fields on that type to the
  //  functions returned from those fields (and including whether it is a command)
  // TODO - this must be versioned!
  // TODO - how to include commands as well
  // This is a map from NewMapType => (String => NewMapObject)
  typeToFieldMapping: UntaggedObject = UMap(Vector.empty),

  // Also a stored versioned type, but a special one!
  typeSystem: NewMapTypeSystem = NewMapTypeSystem(),
  channelIdToType: Map[String, NewMapType] = Map.empty,

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
      typeName <- typeSystem.typeToParameterType.keys.toVector

      parameterTypeOpt = typeSystem.typeToParameterType.get(typeName)

      parameterTypeString = parameterTypeOpt.map(x => "\t" + x).getOrElse("")
    } {
      builder.append(s"${typeName}${parameterTypeString}\n")
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
            UArray(UIdentifier(s), UCase(nType.asUntagged, uObject))
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
          case Failure(_) => throw new Exception("FullEnvironmentCommand: " + nObject.displayString(this))
        }
      }
      case NewVersionedStatementCommand(s, nType) => {
        val uuid = java.util.UUID.randomUUID

        val defaultOutcome = UpdateCommandCalculator.getDefaultValueOfCommandType(
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
      case NewVersionedFieldCommand(id, mapT, value, isCommand) => {
        val resultO = for {
          typeTransform <- mapT match {
            case MapT(typeTransform, _) => Success(typeTransform)
            case _ => Failure("Cannot get type transform: mapT")
          }

          inputType = typeTransform.keyType.asUntagged
          outputType = typeTransform.valueType.asUntagged

          currentFields <- Evaluator.applyFunction(typeToFieldMapping, inputType, this, TypeMatcher)

          isCommandU = if (isCommand) UIndex(1) else UIndex(0)

          newFieldMapping = (UIdentifier(id), UArray(UCase(outputType, value), isCommandU))

          newFieldMappings <- currentFields match {
            case UMap(fieldMappings) => Success(newFieldMapping +: fieldMappings)
            case UInit => Success(Vector(newFieldMapping))
            case _ => Failure("unexpected currentFields: " + currentFields)
          }

          newTypeToFieldMapping <- typeToFieldMapping match {
            case UMap(mappings) => {
              val newMappings: Vector[(UntaggedObject, UntaggedObject)] = (inputType -> UMap(newFieldMappings)) +: mappings  
              Success(UMap(newMappings))
            }
            case _ => Failure("unexpected typeToFieldMapping: " + typeToFieldMapping)
          }
        } yield {
          this.copy(
            typeToFieldMapping = newTypeToFieldMapping
          )
        }

        resultO.toOption.get
      }
      case NewTypeCommand(s, nType) => {
        val parameterType = NewMapTypeSystem.emptyStruct
        val parameterPattern = UArray()

        val newTypeSystem = typeSystem.createNewCustomType(s, parameterType, parameterPattern, nType) match {
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
          UWildcard(paramList.head._1)
        } else {
          val arr: Array[UntaggedObject] = paramList.toArray.map(param => UWildcard(param._1))
          UArray(arr)
        }

        val paramT = if (paramList.length == 1) {
          paramList.head._2
        } else {
          StructT(
            UMap(paramList.zipWithIndex.map(x => UIndex(x._2) -> x._1._2.asUntagged)),
            IndexT(UIndex(paramList.length))
          )
        }

        val newTypeSystem = typeSystem.createNewCustomType(s, paramT, parameterPattern, nType) match {
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

        val parameterType = NewMapTypeSystem.emptyStruct

        val newTypeSystem = typeSystem.createNewCustomType(s, parameterType, UArray(), nType) match {
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
                
                inputT <- UpdateCommandCalculator.getCommandInputOfCommandType(nType, this)
                convertedCommand <- TypeConverter.attemptConvertObjectToType(commandObj, inputT, this)
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
              newValue <- UpdateCommandCalculator.updateVersionedObject(currentState, command, this)
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
            for {
              currentUnderlyingType <- typeSystem.currentUnderlyingType(s)
              response <- TypeExpander.expandType(currentUnderlyingType._2, command, this)
              newTypeSystem <- typeSystem.upgradeCustomType(s, response.newType, response.converter)
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
        val uType = nObject.nType.asUntagged
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
        val taggedObject = NewMapObject(nObject, CustomT("String", UArray(), typeSystem.currentVersion))

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
    val uType = nType.asUntagged
    NewMapObject(uType, TypeT)
  }

  def TypeTransform(
    inputT: NewMapType,
    outputT: NewMapType
  ): UntaggedObject = {
    USingularMap(inputT.asUntagged, outputT.asUntagged)
  }
}

object Environment {
  def eCommand(id: String, nObject: NewMapObject): EnvironmentCommand = {
    FullEnvironmentCommand(id, nObject)
  }

  def fullFuncT(typeTransform: TypeTransform): NewMapType = {
    MapT(typeTransform, MapConfig(RequireCompleteness, FullFunction))
  }

  def structTypeFromParams(params: Vector[(String, NewMapType)]) = {
    val paramsToObject = {
      params.map(x => UIdentifier(x._1) -> x._2.asUntagged)
    }

    StructT(UMap(paramsToObject), IdentifierT)
  }

  def caseTypeFromParams(params: Vector[(String, NewMapType)]) = {
    val paramsToObject = {
      params.map(x => UIdentifier(x._1) -> x._2.asUntagged)
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

  // TODO - can we say that these are already in the type name space, so we can remove these?
  def typeAsObject(nType: NewMapType): NewMapObject = {
    Base.typeAsObject(nType)
  }

  var Base: Environment = Environment()

  def buildSimpleMapT(typeTransform: TypeTransform): UntaggedObject = {
    val config = MapConfig(RequireCompleteness, SimpleFunction)
    val nType = MapT(typeTransform, config)
    nType.asUntagged
  }

  def buildSubtypeT(isMember: UntaggedObject, parentType: UntaggedObject): UntaggedObject = {
    UCase(UIdentifier("Subtype"), UArray(Array(
      isMember,
      parentType,
      UIdentifier("BasicMap")
    )))
  }

  def buildMapCreator(config: MapConfig, allowGenerics: Boolean): NewMapObject = {
    val transformMapT = {
      UCase(UIdentifier("Map"), UArray(Array(
        ParamId("typeTransform"),
        NewMapType.mapConfigToUntagged(config)
      )))
    }

    NewMapObject(
      USingularMap(UWildcard("typeTransform"), transformMapT),
      MapT(
        TypeTransform(TypeTransformT(allowGenerics), TypeT),
        MapConfig(RequireCompleteness, PatternMap)
      )
    )
  }

  Base = Base.newCommands(Vector(
    eCommand("Type", typeAsObject(TypeT)),
    eCommand("Count", typeAsObject(CountT)),
    eCommand("Char", typeAsObject(CharacterT)),
    eCommand("Identifier", typeAsObject(IdentifierT)),
    eCommand("Increment", NewMapObject(
      UMap(Vector(UWildcard("i") -> UCase(UIdentifier("Inc"), ParamId("i")))),
      MapT(TypeTransform(CountT, CountT), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("IsCommand", NewMapObject(
      IsCommandFunc,
      MapT(TypeTransform(TypeT, BooleanT), MapConfig(CommandOutput, SimpleFunction))
    )),
    eCommand("Boolean", typeAsObject(BooleanT)),
    eCommand("Sequence", NewMapObject(
      USingularMap(
        UWildcard("key"), 
        buildSimpleMapT(TypeTransform(IndexT(UIndex(0)),ParamIdT("key")))
      ),
      MapT(TypeTransform(TypeT, TypeT), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("Map", buildMapCreator(MapConfig(CommandOutput, BasicMap), false)),
    eCommand("GenericMap", buildMapCreator(MapConfig(RequireCompleteness, SimpleFunction), true)),
    eCommand("ReqMap", buildMapCreator(MapConfig(RequireCompleteness, SimpleFunction), true)),
    eCommand("Table", buildMapCreator(MapConfig(RequireCompleteness, SimpleFunction), true)),
    eCommand("CaseType", typeAsObject(CaseT(UMap(Vector.empty), IdentifierT, BasicMap))),
    eCommand("StructSeq", typeAsObject(StructT(UMap(Vector.empty), IndexT(UIndex(0))))),
    eCommand("Subtype", NewMapObject(
      UMap(Vector(UWildcard("t") -> buildSubtypeT(UMap(Vector.empty), ParamId("t")))),
      MapT(TypeTransform(TypeT, TypeT), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    NewTypeClassCommand("_default", USingularMap(UWildcard("t"), ParamId("t"))),
    NewTypeClassCommand("_typeOf", 
      USingularMap(
        UWildcard("t"),
        buildSimpleMapT(TypeTransform(WildcardT("_"), TypeT))
      )
    ),
    ApplyIndividualCommand(
      "_typeOf",
      UMap(Vector(UWildcard("t") -> UMap(Vector(UWildcard("_") -> ParamId("t")))))
    ),
    NewParamTypeCommand(
      "Array",
      Vector("T" -> TypeT),
      ArrayT(ParamIdT("T"))
    ),
    NewTypeCommand("Object", NewMapO.taggedObjectT),
    NewVersionedStatementCommand("__FunctionSystem", FunctionalSystemT(Vector.empty)),
    NewTypeClassCommand(
      "Addable",
      USingularMap(
        USingularMap(IndexT(UIndex(2)).asUntagged, UWildcard("t")),
        ParamId("t")
      )
    ),
    eCommand("+", NewMapObject(
      UPlus,
      MapT(TypeTransform(
        MapT(TypeTransform(IndexT(UIndex(2)), CountT), MapConfig(RequireCompleteness, BasicMap)),
        CountT
      ), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("*", NewMapObject(
      UTimes,
      MapT(TypeTransform(
        MapT(TypeTransform(IndexT(UIndex(2)), DoubleT), MapConfig(RequireCompleteness, BasicMap)),
        DoubleT
      ), MapConfig(RequireCompleteness, SimpleFunction))
    )),
    eCommand("/", NewMapObject(
      UDivide,
      MapT(TypeTransform(
        MapT(TypeTransform(IndexT(UIndex(2)), DoubleT), MapConfig(RequireCompleteness, BasicMap)),
        DoubleT
      ), MapConfig(RequireCompleteness, SimpleFunction))
    ))
  ))

  // TODO - eventually make this empty and add it elsewhere!!
  val initialChannelToType = Map(
    "stdout" -> CustomT("String", UArray(), 10) // WHY 10?
  )
  Base = Base.copy(channelIdToType = initialChannelToType)
}