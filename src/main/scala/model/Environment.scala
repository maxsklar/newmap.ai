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
      }
    }

    Environment(newCommands, newObjectMap)
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
    FullEnvironmentCommand("Identifier", TypeT, IdentifierType),
    FullEnvironmentCommand("Map", LambdaT(
      params = Vector(
        "key" -> TypeT,
        "value" -> TypeT,
        "default" -> SubstitutableT("value")
      ),
      result = TypeT
    ), LambdaInstance(
      params = Vector(
        "key" -> TypeType,
        "value" -> TypeType,
        "default" -> ParameterObj("value")
      ),
      expression = MapType(
        ParameterObj("key"),
        ParameterObj("value"),
        ParameterObj("default")
      )
    )),    
    FullEnvironmentCommand("Struct", LambdaT(
      params = Vector(
        "params" -> MapT(IdentifierT, TypeT, Index(1))
      ),
      result = TypeT
    ), LambdaInstance(
      params = Vector(
        "params" -> MapType(IdentifierType, TypeType, Index(1))
      ),
      expression = StructType(
        ParameterObj("params")
      )
    )),
    FullEnvironmentCommand("Subtype", LambdaT(
      params = Vector(
        "parent" -> TypeT
      ),
      result = TypeT // Not only is it a type, but it's a type of types. TODO: formalize this
    ), LambdaInstance(
      params = Vector(
        "parent" -> TypeType
      ),
      expression = SubtypeType(
        ParameterObj("parent")
      )
    ))
  ))

  def paramToEnvCommand(x: (String, NewMapType)): FullEnvironmentCommand = {
    FullEnvironmentCommand(x._1, x._2, ParameterObj(x._1))
  }
}

