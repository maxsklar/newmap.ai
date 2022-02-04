package ai.newmap.model

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.ListMap
import ai.newmap.interpreter._ //TODO: Remove this dependence
import ai.newmap.util.{Outcome, Success, Failure}

// TODO: will these also have (optionally?) a hash value
sealed abstract class EnvironmentCommand

case class FullEnvironmentCommand(
  id: NewMapObject,
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
      val command = FullEnvironmentCommand(IdentifierInstance(id), nObject)
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
        case FullEnvironmentCommand(IdentifierInstance(s), nObject) => {
          idToObject + (s -> nObject)
        }
        case FullEnvironmentCommand(id, nObject) => {
          // TODO - flesh this out!
          throw new Exception("invalid id type: $id")
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

  def newParams(xs: Vector[(NewMapObject, NewMapSubtype)]) = {
    // TODO: deal with this issue better
    val paramsWithString = xs.map(x => x match {
      case (IdentifierInstance(s), t) => Some((s, t))
      case _ => None
    }).flatten

    newCommands(paramsWithString.map(Environment.paramToEnvCommand))
  }
}

object Environment {
  def eCommand(id: String, nObject: NewMapObject): EnvironmentCommand = {
    FullEnvironmentCommand(IdentifierInstance(id), nObject)
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
        val command = FullEnvironmentCommand(IdentifierInstance(id), nObject)
        println(command.toString)
      }
    }
  }

  def i(s: String): NewMapObject = IdentifierInstance(s)


  val Base: Environment = Environment().newCommands(Vector(
    eCommand("Type", TypeT(0)),
    eCommand("Count", CountT),
    eCommand("Identifier", IdentifierT),
    eCommand("Map", LambdaInstance(
      paramStrategy = StructParams(Vector(
        i("key") -> TypeT(0),
        i("value") -> NewMapO.commandT(0)
      )),
      expression = MapT(
        SubstitutableT("key", TypeT(0)),
        SubstitutableT("value", NewMapO.commandT(0)),
        CommandOutput,
        BasicMap
      )
    )),
    eCommand("ReqMap", LambdaInstance(
      paramStrategy = StructParams(Vector(
        i("key") -> TypeT(0),
        i("value") -> TypeT(0)
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
        i("fieldType") -> TypeT(0),
        i("structParams") -> MapT(SubstitutableT("fieldType", TypeT(0)), TypeT(0), RequireCompleteness, BasicMap)
      )),
      expression = StructT(
        ParameterObj(
          "structParams",
          MapT(
            SubstitutableT("fieldType", TypeT(0)),
            TypeT(0),
            RequireCompleteness,
            SimpleFunction
          )
        )
      )
    )),
    // TODO: Case Commands must be added back in
    eCommand("Case", LambdaInstance(
      paramStrategy = StructParams(Vector(
        i("casesType") -> TypeT(0),
        i("cases") -> MapT(SubstitutableT("casesType", TypeT(0)), TypeT(0), RequireCompleteness, BasicMap)
      )),
      expression = CaseT(
        ParameterObj(
          "cases",
          MapT(
            SubstitutableT("cases", TypeT(0)),
            TypeT(0),
            RequireCompleteness,
            SimpleFunction
          )
        )
      )
    )),
    eCommand("Subtype", LambdaInstance(
      paramStrategy = StructParams(Vector(
        i("keyType") -> TypeT(0),
        i("valueType") -> TypeT(0),
        i("simpleFunction") -> MapT(SubstitutableT("keyType", TypeT(0)), SubstitutableT("valueType", TypeT(0)), CommandOutput, SimpleFunction)
      )),
      expression = SubtypeT(
        ParameterObj(
          "simpleFunction",
          MapT(
            SubstitutableT("keyType", TypeT(0)),
            SubstitutableT("valueType", TypeT(0)),
            RequireCompleteness,
            SimpleFunction
          )
        )
      )
    ))
  ))
  
  def paramToEnvCommand(x: (String, NewMapSubtype)): EnvironmentCommand = {
    eCommand(x._1, ParameterObj(x._1, x._2))
  }
}

