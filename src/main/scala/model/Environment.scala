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

  // TODO: perhaps this should take NewMapObject
  def newParam(id: String, nType: NewMapType): Environment = {
    newCommand(Environment.paramToEnvCommand((id, nType)))
  }

  // TODO: perhaps this should take NewMapObject
  def newParams(xs: Vector[(String, NewMapType)]) = {
    newCommands(xs.map(Environment.paramToEnvCommand))
  }
}

object Environment {
  def eCommand(id: String, nType: NewMapType, nObject: NewMapObject): EnvironmentCommand = {
    FullEnvironmentCommand(id, NewMapObjectWithType.withTypeE(nObject, nType))
  }

  def simpleFuncT(inputType: NewMapType, outputType: NewMapType): NewMapType = {
    MapT(inputType, outputType, RequireCompleteness, BasicMap)
  }

  def structTypeFromParams(params: Vector[(String, NewMapType)]) = {
    val fieldType = {
      Subtype(
        IdentifierT,
        MapInstance(params.map(x => IdentifierInstance(x._1) -> Index(1)))
      )
    }

    val paramsToObject = {
      params.map(x => IdentifierInstance(x._1) -> x._2)
    }

    StructT(fieldType, MapInstance(paramsToObject))
  }

  def caseTypeFromParams(params: Vector[(String, NewMapType)]) = {
    val fieldType = {
      Subtype(
        IdentifierT,
        MapInstance(params.map(x => IdentifierInstance(x._1) -> Index(1)))
      )
    }

    val paramsToObject = {
      params.map(x => IdentifierInstance(x._1) -> x._2)
    }

    CaseT(fieldType, MapInstance(paramsToObject))
  }

  // For Debugging
  def printEnvWithoutBase(env: Environment): Unit = {
    for ((id, objWithTypeInfo) <- env.idToObjectWithType) {
      if (!Base.idToObjectWithType.contains(id)) {
        val command = FullEnvironmentCommand(id, objWithTypeInfo)
        println(command.toString)
      }
    }
  }


  val Base: Environment = Environment().newCommands(Vector(
    eCommand("Type", TypeT, TypeT),
    eCommand("Count", TypeT, CountT),
    eCommand("Identifier", TypeT, IdentifierT),
    eCommand("Map", simpleFuncT(
      structTypeFromParams(
        Vector(
          "key" -> TypeT,
          "value" -> CommandTypeT
        )
      ),
      TypeT
    ), LambdaInstance(
      paramStrategy = StructParams(Vector(
        "key" -> TypeT,
        "value" -> TypeT
      )),
      expression = MapT(
        SubstitutableT("key"),
        SubstitutableT("value"),
        CommandOutput,
        BasicMap
      )
    )),
    eCommand("ReqMap", simpleFuncT(
      structTypeFromParams(Vector(
        "key" -> TypeT,
        "value" -> TypeT
      )),
      TypeT
    ), LambdaInstance(
      paramStrategy = StructParams(Vector(
        "key" -> TypeT,
        "value" -> TypeT
      )),
      expression = MapT(
        SubstitutableT("key"),
        SubstitutableT("value"),
        RequireCompleteness,
        SimpleFunction
      )
    )),
    
    eCommand("Struct", simpleFuncT(
      structTypeFromParams(Vector(
        "fieldType" -> TypeT,
        "structParams" -> MapT(SubstitutableT("fieldType"), TypeT, RequireCompleteness, BasicMap)
      )),
      TypeT
    ), LambdaInstance(
      paramStrategy = StructParams(Vector(
        "fieldType" -> TypeT,
        "structParams" -> MapT(SubstitutableT("fieldType"), TypeT, RequireCompleteness, BasicMap)
      )),
      expression = StructT(
        SubstitutableT("fieldType"),
        ParameterObj("structParams")
      )
    )),
    // TODO: Case Commands must be added back in
    eCommand("Case", simpleFuncT(
      structTypeFromParams(Vector(
        "casesType" -> TypeT,
        "caseToType" -> MapT(SubstitutableT("casesType"), TypeT, RequireCompleteness, BasicMap)
      )),
      TypeT
    ), LambdaInstance(
      paramStrategy = StructParams(Vector(
        "casesType" -> TypeT,
        "caseToType" -> MapT(SubstitutableT("casesType"), TypeT, RequireCompleteness, BasicMap)
      )),
      expression = CaseT(
        SubstitutableT("casesType"),
        ParameterObj("caseToType")
      )
    )),
    eCommand("Subtype", simpleFuncT(
      //TODO: this key type and value type are annoying - replace with generics when we can!!
      structTypeFromParams(Vector(
        "keyType" -> TypeT,
        "valueType" -> TypeT,
        "simpleFunction" -> MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), CommandOutput, SimpleFunction)
      )),
      TypeT // Not only is it a type, but it's a type of types. TODO: formalize this
    ), LambdaInstance(
      paramStrategy = StructParams(Vector(
        "keyType" -> TypeT,
        "valueType" -> TypeT,
        "simpleFunction" -> MapT(SubstitutableT("keyType"), SubstitutableT("valueType"), CommandOutput, SimpleFunction)
      )),
      expression = Subtype(
        SubstitutableT("keyType"),
        ParameterObj("simpleFunction")
      )
    ))
  ))
  def paramToEnvCommand(x: (String, NewMapType)): EnvironmentCommand = {
    eCommand(x._1, x._2, ParameterObj(x._1))
  }
}

