package ai.newmap.model

// This class represents the type of commands that you can give to the environment in order to change the state
sealed abstract class EnvironmentCommand {
  def displayString(env: Environment): String
  override def toString(): String = displayString(Environment.Base)
}

case class FullEnvironmentCommand(
  id: String,
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"val $id: ${nObject.nType.displayString(env)} = ${PrintNewMapObject.untagged(nObject.uObject)}"
  }
}

case class NewVersionedStatementCommand(
  id: String,
  nType: NewMapType
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"ver $id = new ${PrintNewMapObject.newMapType(nType, env.typeSystem)}"
  }
}

case class NewVersionedFieldCommand(
  id: String,
  mapT: NewMapType,
  value: UntaggedObject,
  isCommand: Boolean
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"new ${mapT.displayString(env)} as $id = $value"
  }
}

case class NewTypeCommand(
  id: String,
  nType: NewMapType
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"data $id = ${PrintNewMapObject.newMapType(nType, env.typeSystem)}"
  }
}

case class NewParamTypeCommand(
  id: String,
  paramList: Vector[(String, NewMapType)],
  nType: NewMapType
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"data $id ${paramList}"
  }
}

case class NewTypeClassCommand(
  id: String,
  isMember: UntaggedObject
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"typeclass $id ${isMember}"
  }
}

case class UpdateTypeclassWithTypeCommand(
  id: String,
  nType: NewMapType,
  implementations: Vector[(String, UntaggedObject)]
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"update typeclass $id with type $nType $implementations"
  }
}

case class UpdateTypeclassWithFieldCommand(
  id: String,
  typeParameter: String,
  fieldType: NewMapType,
  fieldName: String,
  implementations: Vector[(NewMapType, UntaggedObject)],
  isCommand: Boolean
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"update typeclass $id $typeParameter with field $fieldType as $fieldName ${UMap(implementations.map(x => x._1.asUntagged -> x._2))}"
  }
}

case class IterateIntoCommand(
  iterableObject: NewMapObject,
  destinationObject: String
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"iterate ${iterableObject.displayString(env)} into ${destinationObject}"
  }
}

case class ApplyIndividualCommand(
  id: String,
  nObject: UntaggedObject
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"update $id $nObject"
  }
}

case class ForkEnvironmentCommand(
  id: String,
  vObject: VersionedObjectKey
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = {
    s"ver $id = fork ${vObject}"
  }
}

case class ExpOnlyEnvironmentCommand(
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = nObject.displayString(env)
}

case class AddChannel(
  channel: UntaggedObject,
  nType: NewMapType
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = s"addChannel $channel ${nType.displayString(env)}"
}

case class ConnectChannel(
  channel: UntaggedObject,
  versionedObject: String
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = s"connectChannel $channel $versionedObject}"
}

case class DisconnectChannel(
  channel: UntaggedObject,
  versionedObject: String
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = s"disconnectChannel $channel $versionedObject}"
}

// These are "side effects"
// In some environments, these sides effect are "piped" to other objects
case class OutputToChannel(
  nObject: UntaggedObject,
  channel: UntaggedObject
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = s"write $nObject to $channel"
}

// This should be generalized as "output to a file handle" but for now, just stdout
// We will not call this directly, only from the stdout channel
case class OutputToStdout(
  nObject: UntaggedObject
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = s"write $nObject"
}

case class IterateIntoChannel(
  nObject: UntaggedObject,
  channel: UntaggedObject
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = s"iterate $nObject into $channel"
}

case class AddTypeConversion(
  nTypeFrom: NewMapType,
  nTypeTo: NewMapType,
  func: UntaggedObject
) extends EnvironmentCommand {
  override def displayString(env: Environment): String = s"convert ${nTypeFrom.displayString(env)} to ${nTypeTo.displayString(env)} with $func"
}

case object EmptyEnvironmentCommand extends EnvironmentCommand {
  override def displayString(env: Environment): String = ""
}