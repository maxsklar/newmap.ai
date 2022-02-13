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
  ): Outcome[NewMapObject, String] = {
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

  // TODO - should we ensure that nType is actually a type?
  def newParam(id: String, nType: NewMapObject): Environment = {
    newCommand(Environment.paramToEnvCommand((id, nType)))
  }

  def newParams(xs: Vector[(NewMapObject, NewMapObject)]) = {
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

  def simpleFuncT(inputType: NewMapObject, outputType: NewMapObject): NewMapObject = {
    MapT(inputType, outputType, RequireCompleteness, BasicMap)
  }

  def fullFuncT(inputType: NewMapObject, outputType: NewMapObject): NewMapObject = {
    MapT(inputType, outputType, RequireCompleteness, FullFunction)
  }

  def structTypeFromParams(params: Vector[(String, NewMapObject)]) = {
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

    StructT(MapInstance(paramsToObject, MapT(fieldType, TypeT, RequireCompleteness, BasicMap)))
  }

  def caseTypeFromParams(params: Vector[(String, NewMapObject)]) = {
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

    CaseT(MapInstance(paramsToObject, MapT(fieldType, TypeT, RequireCompleteness, BasicMap)))
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

  // Somewhat complex for now, but this is how a pattern/function definition is built up!
  // In code, this should be done somewhat automatically
  def buildDefinitionWithParameters(
    inputs: Vector[(String, NewMapObject)], // A map from parameters and their type
    expression: NewMapObject 
  ): NewMapObject = {
    val Fields = MapInstance(
      inputs.map(x => i(x._1) -> Index(1)),
      MapT(IdentifierT, NewMapO.rangeT(2), RequireCompleteness, SimpleFunction)
    )

    val structT = StructT(
      MapInstance(
        inputs.map(x => i(x._1) -> x._2),
        MapT(Fields, TypeT, RequireCompleteness, SimpleFunction)
      )
    )

    val structI = StructInstance(
      inputs.map(x => IdentifierInstance(x._1) -> IdentifierPattern(x._1, x._2)),
      structT
    )

    MapInstance(
      values = Vector(structI -> expression),
      mapType = MapT(structT, TypeT, RequireCompleteness, SimpleFunction)
    )
  }

  val Base: Environment = Environment().newCommands(Vector(
    eCommand("Any", AnyT),
    eCommand("Type", TypeT),
    eCommand("Count", CountT),
    eCommand("Identifier", IdentifierT),
    eCommand("Increment", IncrementFunc),
    eCommand("Map", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> NewMapO.commandT),
      MapT(
        ParameterObj("key", TypeT),
        ParameterObj("value", NewMapO.commandT),
        CommandOutput,
        BasicMap
      )
    )),
    eCommand("ReqMap", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> TypeT),
      MapT(
        ParameterObj("key", TypeT),
        ParameterObj("value", TypeT),
        RequireCompleteness,
        SimpleFunction
      )
    )),
    eCommand("Struct", LambdaInstance(
      paramStrategy = StructParams(Vector(
        i("fieldType") -> TypeT,
        i("structParams") -> MapT(ParameterObj("fieldType", TypeT), TypeT, RequireCompleteness, BasicMap)
      )),
      expression = StructT(
        ParameterObj(
          "structParams",
          MapT(
            ParameterObj("fieldType", TypeT),
            TypeT,
            RequireCompleteness,
            SimpleFunction
          )
        )
      )
    )),
    // TODO: Case Commands must be added back in
    eCommand("Case", LambdaInstance(
      paramStrategy = StructParams(Vector(
        i("casesType") -> TypeT,
        i("cases") -> MapT(ParameterObj("casesType", TypeT), TypeT, RequireCompleteness, BasicMap)
      )),
      expression = CaseT(
        ParameterObj(
          "cases",
          MapT(
            ParameterObj("cases", TypeT),
            TypeT,
            RequireCompleteness,
            SimpleFunction
          )
        )
      )
    )),
    eCommand("Subtype", LambdaInstance(
      paramStrategy = StructParams(Vector(
        i("keyType") -> TypeT,
        i("valueType") -> TypeT,
        i("simpleFunction") -> MapT(ParameterObj("keyType", TypeT), ParameterObj("valueType", TypeT), CommandOutput, SimpleFunction)
      )),
      expression = SubtypeT(
        ParameterObj(
          "simpleFunction",
          MapT(
            ParameterObj("keyType", TypeT),
            ParameterObj("valueType", TypeT),
            RequireCompleteness,
            SimpleFunction
          )
        )
      )
    ))
  ))
  
  def paramToEnvCommand(x: (String, NewMapObject)): EnvironmentCommand = {
    eCommand(x._1, ParameterObj(x._1, x._2))
  }
}