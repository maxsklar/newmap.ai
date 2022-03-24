package ai.newmap.model

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.ListMap
import ai.newmap.interpreter._
import ai.newmap.util.{Outcome, Success, Failure}

sealed abstract class EnvironmentCommand

case class FullEnvironmentCommand(
  id: String,
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = {
    //val nType = RetrieveType(nObject)
    //s"val $id: ${nType} = ${nObject}"

    // TODO include type(as above) using environment

    // TODO - should be different if a versioned object
    nObject match {
      case VersionedObject(state, nType, 0) => {
        "" //s"ver $id = new ${nType}"
      }
      case VersionedObject(state, nType, version) => {
        s"ver $id = ${state}; ver=$version"
      }
      case _ => s"val $id = ${nObject}"
    }
  }
}

case class ApplyIndividualCommand(
  id: String,
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = {
    "" //s"update $id $nObject"
  }
}

case class ParameterEnvironmentCommand(
  id: String,
  nType: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = {
    s"val $id: ${nType}"
  }
}

case class ExpOnlyEnvironmentCommand(
  nObject: NewMapObject
) extends EnvironmentCommand {
  override def toString: String = nObject.toString
}

// Additional things to keep track of: latest versions of all versioned objects??
case class Environment(
  commands: Vector[EnvironmentCommand] = Vector.empty,
  idToObject: ListMap[String, NewMapObject] = ListMap.empty,

  // keep track of all the reqmaps from mutable types, because these must change over time
  reqMapsFromMutableTypes: Vector[NewMapObject] = Vector.empty
) {
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
        case FullEnvironmentCommand(s, nObject) => {
          idToObject + (s -> nObject)
        }
        case ApplyIndividualCommand(s, nObject) => {
          val result = Evaluator.updateVersionedObject(s, nObject, this).toOption.get
          idToObject + (s -> result)
        }
        case ParameterEnvironmentCommand(s, nType) => {
          val uuid = java.util.UUID.randomUUID
          idToObject + (s -> ParameterObj(uuid, nType))
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
    newCommand(ParameterEnvironmentCommand(id, nType))
  }

  def newParams(xs: Vector[(String, NewMapObject)]) = {
    newCommands(xs.map(x => ParameterEnvironmentCommand(x._1, x._2)))
  }
}

object Environment {
  def eCommand(id: String, nObject: NewMapObject): EnvironmentCommand = {
    FullEnvironmentCommand(id, nObject)
  }

  def simpleFuncT(inputType: NewMapObject, outputType: NewMapObject): NewMapObject = {
    MapT(inputType, outputType, RequireCompleteness, BasicMap)
  }

  def fullFuncT(inputType: NewMapObject, outputType: NewMapObject): NewMapObject = {
    MapT(inputType, outputType, RequireCompleteness, FullFunction)
  }

  def structTypeFromParams(params: Vector[(String, NewMapObject)]) = {
    val paramsToObject = {
      params.map(x => ObjectPattern(IdentifierInstance(x._1)) -> x._2)
    }

    StructT(
      MapInstance(
        paramsToObject,
        MapT(IdentifierT, TypeT, SubtypeInput, BasicMap)
      )
    )
  }

  def caseTypeFromParams(params: Vector[(String, NewMapObject)]) = {
    val paramsToObject = {
      params.map(x => ObjectPattern(IdentifierInstance(x._1)) -> x._2)
    }

    CaseT(
      MapInstance(
        paramsToObject,
        MapT(IdentifierT, TypeT, SubtypeInput, SimpleFunction)
      )
    )
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

  def i(s: String): NewMapObject = IdentifierInstance(s)

  // Somewhat complex for now, but this is how a pattern/function definition is built up!
  // In code, this should be done somewhat automatically
  // TODO - change to pure pattern matching!!
  def buildDefinitionWithParameters(
    inputs: Vector[(String, NewMapObject)], // A map from parameters and their type
    expression: NewMapObject 
  ): NewMapObject = {
    val structT = StructT(
      MapInstance(
        inputs.zipWithIndex.map(x => ObjectPattern(Index(x._2)) -> x._1._2),
        MapT(IdentifierT, TypeT, SubtypeInput, SimpleFunction)
      )
    )

    val structP = StructPattern(inputs.map(x => TypePattern(x._1, x._2)))

    MapInstance(
      values = Vector(structP -> expression),
      mapType = MapT(structT, TypeT, RequireCompleteness, SimpleFunction)
    )
  }

  val Base: Environment = Environment().newCommands(Vector(
    eCommand("Any", AnyT),
    eCommand("Type", TypeT),
    eCommand("Count", CountT),
    eCommand("Identifier", IdentifierT),
    eCommand("Increment", IncrementFunc),
    eCommand("IsCommand", IsCommandFunc),
    eCommand("Sequence", MapInstance(
      values = Vector(TypePattern("key", TypeT) -> SequenceT(ParamId("key"))),
      mapType = MapT(TypeT, TypeT, RequireCompleteness, SimpleFunction)
    )),
    eCommand("Map", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> NewMapO.commandT),
      MapT(ParamId("key"), ParamId("value"), CommandOutput, BasicMap)
    )),
    eCommand("ReqMap", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> TypeT),
      MapT(ParamId("key"), ParamId("value"), RequireCompleteness, SimpleFunction)
    )),
    eCommand("SubMap", buildDefinitionWithParameters(
      Vector("key" -> TypeT, "value" -> TypeT),
      MapT(ParamId("key"), ParamId("value"), SubtypeInput, SimpleFunction)
    )),
    eCommand("Struct", MapInstance(
      values = Vector(
        TypePattern("structParams", MapT(IdentifierT, TypeT, SubtypeInput, BasicMap)) -> StructT(ParamId("structParams"))
      ),
      mapType = MapT(
        MapT(IdentifierT, TypeT, SubtypeInput, BasicMap),
        TypeT,
        RequireCompleteness,
        SimpleFunction
      )
    )),
    // TODO: This CStruct is going to be merged with Struct.. once we take care of generics
    eCommand("CStruct", MapInstance(
      values = Vector(
        TypePattern("structParams", MapT(CountT, TypeT, SubtypeInput, BasicMap)) -> StructT(ParamId("structParams"))
      ),
      mapType = MapT(
        MapT(CountT, TypeT, SubtypeInput, BasicMap),
        TypeT,
        RequireCompleteness,
        SimpleFunction
      )
    )),
    // TODO: right now cases must be identifier based, expand this in the future!!
    eCommand("Case", MapInstance(
      values = Vector(
        TypePattern("cases", MapT(IdentifierT, TypeT, SubtypeInput, SimpleFunction)) -> CaseT(ParamId("cases"))
      ),
      mapType = MapT(
        MapT(IdentifierT, TypeT, SubtypeInput, SimpleFunction),
        TypeT,
        RequireCompleteness,
        SimpleFunction
      )
    )),
    eCommand("Subtype", MapInstance(
      values = Vector(
        TypePattern("simpleFunction", NewMapO.simpleFunctionT) -> SubtypeT(ParamId("simpleFunction"))
      ),
      mapType = MapT(
        NewMapO.simpleFunctionT,
        TypeT,
        RequireCompleteness,
        SimpleFunction
      )
    )),
  ))
}