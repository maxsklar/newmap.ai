package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}

class EnvironmentInterpreter() {
  var env: Environment = Environment.Base

  for (code <- EnvironmentInterpreter.initialCommands) {
    apply(code)
  }

  /*
   * @param code The code entered into the interpreter
   * @return The response from the interpreter
   */
  def apply(code: String): Outcome[String, String] = {
    applyInterpCommand(code) match {
      case CommandPrintSomething(response) => Success(response)
      case CommandExit => Success(":exit")
      case CommandPassThrough => applyEnvCommand(code)
    }
  }

  abstract class CommandInterpResponse
  case class CommandPrintSomething(s: String) extends CommandInterpResponse
  case object CommandExit extends CommandInterpResponse
  case object CommandPassThrough extends CommandInterpResponse

  val Pattern = "(:parse )(.*)".r

  def applyInterpCommand(code: String): CommandInterpResponse = {
    code match {
      case ":env" => CommandPrintSomething(env.toString)
      case ":exit" | ":quit" => CommandExit
      case _ if (code.startsWith(":parse ")) => {
        CommandPrintSomething(formatStatementParserCode(code.drop(7)))
      }
      case ":help" => CommandPrintSomething(
        "List of environment commands\n" ++
        ":env\tPrint the current environment\n" ++
        ":parse [Expression]\tPrint how [Expression] is parsed and type checked" ++
        ":exit | :quit\tExit this repl\n" ++
        ":help\tPrint this help message\n"
      )
      case _ => CommandPassThrough
    }
  }

  private def formatStatementParserCode(code: String): String = {
    statementParser(code) match {
      case Success(p) => p.toString
      case Failure(s) => s"Parse Failed: $s"
    }
  }

  private def statementParser(code: String): Outcome[EnvStatementParse, String] = {
    for {
      tokens <- Lexer(code)
      statementParse <- NewMapParser.statementParse(tokens)
    } yield statementParse
  }

  def applyEnvCommand(code: String): Outcome[String, String] = {
    for {
      tokens <- Lexer(code)
      statementParse <- NewMapParser.statementParse(tokens)
      envCommands <- StatementInterpreter(statementParse, env)
    } yield {
      env = env.newCommands(envCommands)
      envCommands.map(_.toString).mkString("\n")
    }
  }
}

object EnvironmentInterpreter {
  // TODO(2022): REDO ALL OF THESE COMMAND TYPES!
  val initialCommands: Vector[String] = Vector(
    "val MutableDescriptor: Type = Struct(versionType: (Count => Type), init: (versionType 0), commandType: Type, update: ((version: Count, current: versionType version, command: commandType) => versionType (increment version)))",
    "val CounterV: MutableDescriptor = (versionType: ((n: Count) => Count), init: 0, commandType: Struct(), update: ((version: Count, current: versionType version, command: commandType) => increment current))",
    "val StackV: ((T: Type, default: T) => MutableDescriptor) = (T: Type, default: T) => (versionType: ((n: Count) => T), init: default, commandType: T, update: ((version: Count, current: versionType version, command: commandType) => command))",
    "val SequenceV: ((T: Type, default: T) => MutableDescriptor) = (T: Type, default: T) => (versionType: ((n: Count) => Map n T default), init: (), commandType: T, update: ((version: Count, current: versionType version, command: commandType) => appendSeq version commandType default current command))",
    "val MapV: ((keyType: Type, valueType: Type, default: valueType) => MutableDescriptor) = (keyType: Type, valueType: Type, default: valueType) => (versionType: ((n: Count) => Map keyType valueType default), init: (), commandType: Map keyType valueType default, update: ((version: Count, current: versionType version, command: commandType) => appendMap keyType valueType default current command))"
  )
}