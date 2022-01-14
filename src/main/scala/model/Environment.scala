package ai.newmap.model

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.ListMap
import ai.newmap.util.{Outcome, Success, Failure}

// TODO: will these also have (optionally?) a hash value
sealed abstract class EnvironmentCommand

case class FullEnvironmentCommand(
  id: String,
  nObjectWithType: NewMapObjectWithType
) extends EnvironmentCommand {
  override def toString: String = nObjectWithType.nTypeInfo match {
    case ExplicitlyTyped(nType) => {
      "val " + id + ": " + nType + " = " + nObjectWithType.nObject
    }
    case ImplicitlyTyped(nTypes) => {
      if (nTypes.length == 0) {
        "val " + id + " = " + nObjectWithType.nObject
      } else {
        // TODO - type intersections still need work
        "val " + id + ": " + "Convertible(" + nTypes.mkString(", ") + ")" + " = " + nObjectWithType.nObject
      }
    }
    
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
    if (lookup(identifier).isEmpty) {
      Thread.dumpStack()
    }

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
      val command = FullEnvironmentCommand(id, objWithTypeInfo)
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
        case FullEnvironmentCommand(id, nObjectWithType) => {
          idToObjectWithType + (id -> nObjectWithType)
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
  def eCommand(id: String, nType: NewMapType, nObject: NewMapObject): EnvironmentCommand = {
    FullEnvironmentCommand(id, NewMapObjectWithType.withTypeE(nObject, nType))
  }

  def simpleFuncT(inputType: NewMapType, outputType: NewMapType): NewMapType = {
    val transformV = Vector(
      ConvertNewMapTypeToObject(inputType) -> ConvertNewMapTypeToObject(outputType)
    )
    val transformer = MapInstance(transformV, Index(0))
    LambdaT(transformer)
  }


  val Base: Environment = Environment().newCommands(Vector(
    eCommand("Type", TypeT, TypeType),
    eCommand("Count", TypeT, CountType),
    eCommand("Identifier", TypeT, IdentifierType),
    eCommand("Map", simpleFuncT(
      StructT(Vector(
        "key" -> TypeT,
        "value" -> TypeT,
        "default" -> SubstitutableT("value")
      )),
      TypeT
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
    eCommand("ReqMap", simpleFuncT(
      StructT(Vector(
        "key" -> TypeT,
        "value" -> TypeT
      )),
      TypeT
    ), LambdaInstance(
      paramStrategy = StructParams(Vector(
        "key" -> TypeType,
        "value" -> TypeType
      )),
      expression = ReqMapType(
        ParameterObj("key"),
        ParameterObj("value")
      )
    )),  
    eCommand("Struct", simpleFuncT(
      MapT(IdentifierT, TypeT, Index(1)),
      TypeT
    ), LambdaInstance(
      paramStrategy = IdentifierParam("input", MapType(IdentifierType, TypeType, Index(1))),
      expression = StructType(
        ParameterObj("input")
      )
    )),
    eCommand("Case", simpleFuncT(
      MapT(IdentifierT, TypeT, Index(0)),
      TypeT
    ), LambdaInstance(
      paramStrategy = IdentifierParam("input", MapType(IdentifierType, TypeType, Index(0))),
      expression = CaseType(
        ParameterObj("input")
      )
    )),
    eCommand("Subtype", simpleFuncT(
      TypeT,
      TypeT // Not only is it a type, but it's a type of types. TODO: formalize this
    ), LambdaInstance(
      paramStrategy = IdentifierParam("input", TypeType),
      expression = SubtypeType(
        ParameterObj("input")
      )
    )),

    eCommand(
      "appendMap",
      simpleFuncT(
        StructT(Vector(
          "keyType" -> TypeT,
          "valueType" -> TypeT,
          "default" -> SubstitutableT("valueType"),
          "currentMap" -> MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), ParameterObj("default")),
          "appendedMap" -> MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), ParameterObj("default"))
        )),
        MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), ParameterObj("default"))
      ),
      LambdaInstance(
        StructParams(Vector(
          "keyType" -> TypeType,
          "valueType" -> TypeType,
          "default" -> ParameterObj("valueType"),
          "currentMap" -> MapType(ParameterObj("keyType"), ParameterObj("valueType"), ParameterObj("default")),
          "appendedMap" -> MapType(ParameterObj("keyType"), ParameterObj("valueType"), ParameterObj("default"))
        )),
        AppendToMap(ParameterObj("currentMap"), ParameterObj("appendedMap"))
      ),
    )
  ))
  def paramToEnvCommand(x: (String, NewMapType)): EnvironmentCommand = {
    eCommand(x._1, x._2, ParameterObj(x._1))
  }
}

