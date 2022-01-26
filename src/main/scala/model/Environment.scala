package ai.newmap.model

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.ListMap
import ai.newmap.interpreter._ //TODO: Remove this dependence
import ai.newmap.util.{Outcome, Success, Failure}

// TODO: will these also have (optionally?) a hash value
sealed abstract class EnvironmentCommand

case class FullEnvironmentCommand(
  id: String,
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = {
    val nType = RetrieveType(nObject)
    s"val $id: ${nType} = ${nObject}"
  }
}

case class ExpOnlyEnvironmentCommand(
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = nObject.toString
}

case class Environment(
  commands: Vector[EnvironmentCommand] = Vector.empty,
  idToObject: ListMap[String, NewMapObject] = ListMap.empty
) {
  def typeOf(
    identifier: String
  ): Outcome[NewMapSubtype, String] = {
    lookup(identifier) match {
      case Some(obj) => Success(RetrieveType(obj))
      case None => Failure(s"Failed to lookup identifier $identifier")
    }
  }

  def lookup(identifier: String): Option[NewMapObject] = {
    idToObject.get(identifier)
  }

  override def toString: String = {
    val builder: StringBuilder = new StringBuilder()
    for ((id, nObject) <- idToObject) {
      val command = FullEnvironmentCommand(id, nObject)
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

    val newObjectMap: ListMap[String, NewMapObject] = {
      command match {
        case FullEnvironmentCommand(id, nObject) => {
          idToObject + (id -> nObject)
        }
        case ExpOnlyEnvironmentCommand(nObject) => {
          // TODO: save this in the result list
          idToObject
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

  // TODO: perhaps this should take NewMapObject
  def newParam(id: String, nType: NewMapSubtype): Environment = {
    newCommand(Environment.paramToEnvCommand((id, nType)))
  }

  // TODO: perhaps this should take NewMapObject
  def newParams(xs: Vector[(String, NewMapSubtype)]) = {
    newCommands(xs.map(Environment.paramToEnvCommand))
  }
}

object Environment {
  def eCommand(id: String, nObject: NewMapObject): EnvironmentCommand = {
    FullEnvironmentCommand(id, nObject)
  }

  def simpleFuncT(inputType: NewMapSubtype, outputType: NewMapSubtype): NewMapSubtype = {
    MapT(inputType, outputType, RequireCompleteness, BasicMap)
  }

  def structTypeFromParams(params: Vector[(String, NewMapSubtype)]) = {
    val fieldType = {
      SubtypeT(
        MapInstance(
          params.map(x => IdentifierInstance(x._1) -> Index(1)),
          MapT(IdentifierT, NewMapO.rangeT(2), CommandOutput, BasicMap)
        )
      )
    }

    val paramsToObject = {
      params.map(x => IdentifierInstance(x._1) -> x._2)
    }

    StructT(MapInstance(paramsToObject, MapT(fieldType, TypeT(0), RequireCompleteness, BasicMap)))
  }

  def caseTypeFromParams(params: Vector[(String, NewMapSubtype)]) = {
    val fieldType = {
      SubtypeT(
        MapInstance(
          params.map(x => IdentifierInstance(x._1) -> Index(1)),
          MapT(IdentifierT, NewMapO.rangeT(2), CommandOutput, BasicMap)
        )
      )
    }

    val paramsToObject = {
      params.map(x => IdentifierInstance(x._1) -> x._2)
    }

    CaseT(MapInstance(paramsToObject, MapT(fieldType, TypeT(0), RequireCompleteness, BasicMap)))
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


  val Base: Environment = Environment().newCommands(Vector(
    eCommand("Type", TypeT(0)),
    eCommand("Count", CountT),
    eCommand("Identifier", IdentifierT),
    eCommand("Map", LambdaInstance(
      paramStrategy = StructParams(Vector(
        "key" -> TypeT(0),
        "value" -> TypeT(0)
      )),
      expression = MapT(
        SubstitutableT("key", TypeT(0)),
        SubstitutableT("value", TypeT(0)),
        CommandOutput,
        BasicMap
      )
    )),
    eCommand("ReqMap", LambdaInstance(
      paramStrategy = StructParams(Vector(
        "key" -> TypeT(0),
        "value" -> TypeT(0)
      )),
      expression = MapT(
        SubstitutableT("key", TypeT(0)),
        SubstitutableT("value", TypeT(0)),
        RequireCompleteness,
        SimpleFunction
      )
    )),
    
    eCommand("Struct", LambdaInstance(
      paramStrategy = StructParams(Vector(
        "fieldType" -> TypeT(0),
        "structParams" -> MapT(SubstitutableT("fieldType", TypeT(0)), TypeT(0), RequireCompleteness, BasicMap)
      )),
      expression = StructT(
        ParameterFunc(
          "structParams",
          SubstitutableT("fieldType", TypeT(0)),
          TypeT(0)
        )
      )
    )),
    // TODO: Case Commands must be added back in
    eCommand("Case", LambdaInstance(
      paramStrategy = StructParams(Vector(
        "casesType" -> TypeT(0),
        "cases" -> MapT(SubstitutableT("casesType", TypeT(0)), TypeT(0), RequireCompleteness, BasicMap)
      )),
      expression = CaseT(
        ParameterFunc(
          "cases",
          SubstitutableT("cases", TypeT(0)),
          TypeT(0)
        )
      )
    )),
    eCommand("Subtype", LambdaInstance(
      paramStrategy = StructParams(Vector(
        "keyType" -> TypeT(0),
        "valueType" -> TypeT(0),
        "simpleFunction" -> MapT(SubstitutableT("keyType", TypeT(0)), SubstitutableT("valueType", TypeT(0)), CommandOutput, SimpleFunction)
      )),
      expression = SubtypeT(
        ParameterFunc("simpleFunction", SubstitutableT("keyType", TypeT(0)), SubstitutableT("valueType", TypeT(0)))
      )
    ))
  ))
  def paramToEnvCommand(x: (String, NewMapSubtype)): EnvironmentCommand = {
    eCommand(x._1, ParameterObj(x._1, x._2))
  }
}

