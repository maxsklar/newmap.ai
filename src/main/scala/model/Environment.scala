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
    //if (lookup(identifier).isEmpty) {
    //  Thread.dumpStack()
    //}

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


  val Base: Environment = Environment().newCommands(Vector(
    eCommand("Type", TypeT, TypeType),
    eCommand("Count", TypeT, CountType),
    eCommand("Identifier", TypeT, IdentifierType),
    eCommand("Map", LambdaT(
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
    eCommand("Struct", LambdaT(
      input = MapT(IdentifierT, TypeT, Index(1)),
      result = TypeT
    ), LambdaInstance(
      paramStrategy = IdentifierParam("input", MapType(IdentifierType, TypeType, Index(1))),
      expression = StructType(
        ParameterObj("input")
      )
    )),
    eCommand("Case", LambdaT(
      input = MapT(IdentifierT, TypeT, Index(0)),
      result = TypeT
    ), LambdaInstance(
      paramStrategy = IdentifierParam("input", MapType(IdentifierType, TypeType, Index(0))),
      expression = CaseType(
        ParameterObj("input")
      )
    )),
    eCommand("Subtype", LambdaT(
      input = TypeT,
      result = TypeT // Not only is it a type, but it's a type of types. TODO: formalize this
    ), LambdaInstance(
      paramStrategy = IdentifierParam("input", TypeType),
      expression = SubtypeType(
        ParameterObj("input")
      )
    )),
    eCommand(
      "increment",
      LambdaT(
        input = CountT,
        result = CountT
      ),
      Increment
    ),
    eCommand(
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
      LambdaInstance(
        paramStrategy = StructParams(Vector(
          "currentSize" -> CountType,
          "valueType" -> TypeType,
          "defaultValue" -> ParameterObj("valueType"),
          "currentSeq" -> MapType(ParameterObj("currentSize"), ParameterObj("valueType"), ParameterObj("defaultValue")),
          "nextValue" -> ParameterObj("valueType")
        )),
        expression = AppendToSeq(ParameterObj("currentSeq"), ParameterObj("nextValue"))
      ),
    ),
    eCommand(
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
      LambdaInstance(
        paramStrategy = StructParams(Vector(
          "keyType" -> TypeType,
          "valueType" -> TypeType,
          "default" -> ParameterObj("valueType"),
          "currentMap" -> MapType(ParameterObj("keyType"), ParameterObj("valueType"), ParameterObj("default")),
          "appendedMap" -> MapType(ParameterObj("keyType"), ParameterObj("valueType"), ParameterObj("default"))
        )),
        expression = AppendToMap(ParameterObj("currentMap"), ParameterObj("appendedMap"))
      ),
    )/*,
    eCommand("VType", Subtype(TypeT), MutableTypeT),
    eCommand(
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
        result = MutableTypeT
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
    ),*/
    /*eCommand(
      "new",
      LambdaT(
        input = MutableTypeT
        result = MutableType(???) // An actual object of type mutable type (will be based on input)
      ), LambdaInstance(

      )
    )*/
  ))
  def paramToEnvCommand(x: (String, NewMapType)): EnvironmentCommand = {
    eCommand(x._1, x._2, ParameterObj(x._1))
  }
}

