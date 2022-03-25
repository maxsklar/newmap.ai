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
      case _ if (code.startsWith(":typeOf ")) => {
        CommandPrintSomething(evaluateAndTypeOf(code.drop(8)))
      }
      case ":uuid" => CommandPrintSomething(java.util.UUID.randomUUID.toString)
      case ":help" => CommandPrintSomething(
        "List of environment commands\n" ++
        ":env\tPrint the current environment\n" ++
        ":parse [Expression]\tPrint how [Expression] is parsed and type checked" ++
        ":typeOf [Expression]\tPrint out the type of the expression" ++
        ":exit | :quit\tExit this repl\n" ++
        ":help\tPrint this help message\n"
      )
      case _ => CommandPassThrough
    }
  }

  private def evaluateAndTypeOf(code: String): String = {
    val result = for {
      tokens <- Lexer(code)
      parseTree <- NewMapParser(tokens)
      tc <- TypeChecker.typeCheck(parseTree, AnyT, env)
      nObject <- Evaluator(tc, env)
    } yield {
      val nType = RetrieveType(nObject, env)
      nType.toString
    }

    result match {
      case Success(s) => s
      case Failure(reason) => reason
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
    "val Byte: Type = 8 => 2",
    "val Char: Type = 16 => 2"
  )
}