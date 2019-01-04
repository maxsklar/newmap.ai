package ai.newmap.model

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.ListMap
import ai.newmap.util.{Outcome, Success, Failure}

// TODO: will these also have (optionally?) a hash value
sealed abstract class EnvironmentCommand

case class FullEnvironmentCommand(
  id: String,
  nType: NewMapType,
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = {
    "val " + id + ": " + nType + " = " + nObject
  }
}

case class TypeInferredEnvironmentCommand(
  id: String,
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = {
    "val " + id + " = " + nObject
  }
}

case class ExpOnlyEnvironmentCommand(
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = nObject.toString
}

case class NewVersionedEnvironmentCommand(
  id: String,
  nObject: NewMapType
) extends EnvironmentCommand {
  override def toString: String = {
    "new " + nObject + " " + id
  }
}

case class Environment(
  commands: Vector[EnvironmentCommand] = Vector.empty,
  idToObjectWithType: ListMap[String, NewMapObjectWithType] = ListMap.empty
) {
  def typeOf(
    identifier: String
  ): Outcome[NewMapTypeInfo, String] = {
    Outcome(
      lookup(identifier).map(_.nTypeInfo),
      "Could not get type from object name " + identifier
    )
  }

  def objectOf(
    identifier: String
  ): Option[NewMapObject] = {
    lookup(identifier).map(_.nObject)
  }

  def lookup(identifier: String): Option[NewMapObjectWithType] = {
    idToObjectWithType.get(identifier)
  }

  override def toString: String = {
    val builder: StringBuilder = new StringBuilder()
    for ((id, objWithTypeInfo) <- idToObjectWithType) {
      val command = objWithTypeInfo.nTypeInfo match {
        // TODO - don't rely on EnvironmentCommand to print
        case ExplicitlyTyped(nType) => FullEnvironmentCommand(id, nType, objWithTypeInfo.nObject)
        // TODO - the inferred types must be brought in here somehow
        case ImplicitlyTyped(types) => TypeInferredEnvironmentCommand(id, objWithTypeInfo.nObject)
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

    val newObjectMap: ListMap[String, NewMapObjectWithType] = {
      command match {
        case FullEnvironmentCommand(id, nType, nObject) => {
          idToObjectWithType + (id -> NewMapObjectWithType(nObject, ExplicitlyTyped(nType)))
        }
        case TypeInferredEnvironmentCommand(id, nObject) => {
          idToObjectWithType + (id -> NewMapObjectWithType(nObject, NewMapTypeInfo.init))          
        }
        case ExpOnlyEnvironmentCommand(nObject) => {
          // TODO: save this in the result enum
          idToObjectWithType
        }
        case NewVersionedEnvironmentCommand(id, nType) => {
          createNewMutableObject(nType) match {
            case Some(binding) => idToObjectWithType + (id -> binding)
            case None => idToObjectWithType
          } 
        }
      }
    }

    Environment(newCommands, newObjectMap)
  }

  // TODO - handling errors when this returns None?
  // at least add tests!
  def createNewMutableObject(nType: NewMapType): Option[NewMapObjectWithType] = {
    nType match {
      case MutableT(staticT, init, commandT, updateFunction) => {
        Some(NewMapObjectWithType.withTypeE(MutableObject(Vector.empty, init), nType))
      }
      case _ => None
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
    newCommand(Environment.paramToEnvCommand((id, nType)))
  }

  def newParams(xs: Vector[(String, NewMapType)]) = {
    newCommands(xs.map(Environment.paramToEnvCommand))
  }
}

object Environment {
  val Base: Environment = Environment().newCommands(Vector(
    FullEnvironmentCommand("Type", TypeT, TypeType),
    FullEnvironmentCommand("Count", TypeT, CountType),
    FullEnvironmentCommand("Identifier", TypeT, IdentifierType),
    FullEnvironmentCommand("Map", LambdaT(
      input = StructT(Vector(
        "key" -> TypeT,
        "value" -> TypeT,
        "default" -> SubstitutableT("value")
      )),
      result = TypeT
    ), LambdaInstance(
      paramStrategy = StructParams(Vector(
        "key" -> TypeType,
        "value" -> TypeType,
        "default" -> ParameterObj("value")
      )),
      expression = MapType(
        ParameterObj("key"),
        ParameterObj("value"),
        ParameterObj("default")
      )
    )),    
    FullEnvironmentCommand("Struct", LambdaT(
      input = MapT(IdentifierT, TypeT, Index(1)),
      result = TypeT
    ), LambdaInstance(
      paramStrategy = IdentifierParam("input", MapType(IdentifierType, TypeType, Index(1))),
      expression = StructType(
        ParameterObj("input")
      )
    )),
    FullEnvironmentCommand("Case", LambdaT(
      input = MapT(IdentifierT, TypeT, Index(0)),
      result = TypeT
    ), LambdaInstance(
      paramStrategy = IdentifierParam("input", MapType(IdentifierType, TypeType, Index(0))),
      expression = CaseType(
        ParameterObj("input")
      )
    )),
    FullEnvironmentCommand("Subtype", LambdaT(
      input = TypeT,
      result = TypeT // Not only is it a type, but it's a type of types. TODO: formalize this
    ), LambdaInstance(
      paramStrategy = IdentifierParam("input", TypeType),
      expression = SubtypeType(
        ParameterObj("input")
      )
    )),
    FullEnvironmentCommand(
      "increment",
      LambdaT(
        input = CountT,
        result = CountT
      ),
      Increment
    ),
    FullEnvironmentCommand(
      "appendSeq",
      LambdaT(
        input = StructT(Vector(
          "currentSize" -> CountT,
          "valueType" -> TypeT,
          "defaultValue" -> SubstitutableT("valueType"),
          "currentSeq" -> MapT(SubstitutableT("currentSize"), SubstitutableT("valueType"), ParameterObj("defaultValue")),
          "nextValue" -> SubstitutableT("valueType")
        )),
        result = MapT(IncrementT(SubstitutableT("currentSize")), SubstitutableT("valueType"), ParameterObj("defaultValue"))
      ),
      AppendToSeq
    ),
    FullEnvironmentCommand(
      "appendMap",
      LambdaT(
        input = StructT(Vector(
          "keyType" -> TypeT,
          "valueType" -> TypeT,
          "default" -> SubstitutableT("valueType"),
          "currentMap" -> MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), ParameterObj("default")),
          "appendedMap" -> MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), ParameterObj("default"))
        )),
        result = MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), ParameterObj("default"))
      ),
      AppendToMap
    ),
    FullEnvironmentCommand(
      "VersionedType", 
      LambdaT(
        input = StructT(Vector(
          "staticType" -> TypeT,
          "init" -> SubstitutableT("staticType"),
          "commandType" -> TypeT,
          "apply" -> LambdaT(
            input = StructT(Vector(
              "from" -> SubstitutableT("staticType"),
              "command" -> SubstitutableT("commandType"))),
            result = SubstitutableT("staticType")
          )
        )),
        result = TypeT
      ), LambdaInstance(
        paramStrategy = StructParams(Vector(
          "staticType" -> TypeType,
          "init" -> ParameterObj("staticType"),
          "commandType" -> TypeType,
          "apply" -> LambdaType(
            inputType = StructType(MapInstance(Vector(
              IdentifierInstance("from") -> ParameterObj("staticType"),
              IdentifierInstance("command") -> ParameterObj("commandType")), Index(1))),
            outputType = ParameterObj("staticType")
          )
        )),
        expression = MutableType(
          ParameterObj("staticType"),
          ParameterObj("init"),
          ParameterObj("commandType"),
          ParameterObj("apply")
        )
      )
    )/*,
    FullEnvironmentCommand(
      "new",
      LambdaT(
        input = SubstitutableT("versionedType")
        result = MutableT(

        )
      ), LambdaInstance(

      )
    )*/
  ))
  def paramToEnvCommand(x: (String, NewMapType)): FullEnvironmentCommand = {
    FullEnvironmentCommand(x._1, x._2, ParameterObj(x._1))
  }
}

