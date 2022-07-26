package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}
import java.io.File
import scala.io.Source

class EnvironmentInterpreter(
  useInitialCommands: Boolean = true,
  printInitCommandErrors: Boolean = true,
  suppressStdout: Boolean = false,
) {
  var env: Environment = Environment.Base

  if (suppressStdout) env = env.copy(printStdout = false)

  for (code <- EnvironmentInterpreter.initialCommands) {
    if (useInitialCommands) {
      apply(code) match {
        case Failure(f) => if (printInitCommandErrors) println(s"Error: $f\n => $code")
        case Success(_) => ()
      }
    }
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
      case ":types" => CommandPrintSomething(env.printTypes)
      case ":channels" => CommandPrintSomething(env.printChannels)
      case ":exit" | ":quit" => CommandExit
      case _ if (code.startsWith(":parse ")) => {
        CommandPrintSomething(formatStatementParserCode(code.drop(7)))
      }
      case _ if (code.startsWith(":tokenize ")) => {
        CommandPrintSomething(formatStatementLexerCode(code.drop(10)))
      }
      case _ if (code.startsWith(":underlyingType ")) => {
        CommandPrintSomething(evaluateAndUnderlying(code.drop(16)))
      }
      case _ if (code.startsWith(":load ")) => {
        CommandPrintSomething(loadFile(code.drop(6)))
      }
      case _ if (code.startsWith(":ls ")) => {
        CommandPrintSomething(printDirectory(code.drop(4)))
      }
      case _ if (code.startsWith(":typeOf ")) => {
        CommandPrintSomething(getTypeOf(code.drop(8)))
      }
      case ":uuid" => CommandPrintSomething(java.util.UUID.randomUUID.toString)
      case ":help" => CommandPrintSomething(
        "List of environment commands\n" ++
        ":env\tPrint the current environment\n" ++
        ":types\tPrint the types in the current environment\n" ++
        ":parse [Expression]\tPrint how [Expression] is parsed and type checked\n" ++
        ":tokenize [Expression]\tPrint how [Expression] is Tokenized by running the lexer\n" ++
        ":typeOf [Expression]\tPrint out the type of the expression\n" ++
        ":channels\tPrint the channels in this environment\n" ++
        ":exit | :quit\tExit this repl\n" ++
        ":help\tPrint this help message\n"
      )
      case _ => CommandPassThrough
    }
  }

  private def getTokens(code: String): Outcome[List[Lexer.Token], String] = {
    if (code.isEmpty) Success(List.empty)
    else Lexer(code)
  }

  private def getTypeOf(code: String): String = {
    val result = for {
      tokens <- getTokens(code)
      parseTree <- NewMapParser(tokens)
      tc <- TypeChecker.typeCheckUnknownType(parseTree, env)
      nObject <- Evaluator(tc.nExpression, env)
    } yield {
      tc.refinedTypeClass
    }

    result match {
      case Success(s) => s.displayString(env)
      case Failure(reason) => reason
    }
  }

  private def evaluateAndUnderlying(code: String): String = {
    val result = for {
      tokens <- getTokens(code)
      parseTree <- NewMapParser(tokens)
      tc <- TypeChecker.typeCheck(parseTree, TypeT, env, FullFunction)
      nObject <- Evaluator(tc.nExpression, env)
      nType <- env.typeSystem.convertToNewMapType(nObject)
      underlyingType <- TypeChecker.getFinalUnderlyingType(nType, env, env.typeSystem.currentState)
    } yield {
      underlyingType.displayString(env)
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

  private def formatStatementLexerCode(code: String): String = {
    getTokens(code) match {
      case Success(p) => p.mkString("\t")
      case Failure(s) => s"Tokenizer Failed: $s"
    }
  }

  private def loadFile(filename: String): String = {
    val baseDir = "src/main/newmap/"

    val fileName = baseDir + filename

    val linesIt = Source.fromFile(fileName).getLines

    loadFileFromIterator(linesIt, env) match {
      case Success(_) => ""
      case Failure(reason) => s"Failure: $reason"
    }
  }

  private def loadFileFromIterator(linesIt: Iterator[String], originalEnv: Environment): Outcome[Unit, String] = {
    if (linesIt.hasNext) {
      applyEnvCommand(linesIt.next()) match {
        case Success(result) => loadFileFromIterator(linesIt, originalEnv)
        case Failure(reason) => {
          env = originalEnv
          Failure(reason)
        }
      }
    } else {
      Success(())
    }
  }

  private def printDirectory(dir: String): String = {
    val d = new File(dir)
    val files = if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
    } else {
        List[File]()
    }

    s"directory: $files"
  }

  private def statementParser(code: String): Outcome[EnvStatementParse, String] = {
    for {
      tokens <- getTokens(code)
      statementParse <- NewMapParser.statementParse(tokens)
    } yield statementParse
  }

  def applyEnvCommand(code: String): Outcome[String, String] = {
    for {
      tokens <- getTokens(code)

      // TODO: This statement parse can now be "waiting" for the next line.. keep that in mind
      statementParse <- NewMapParser.statementParse(tokens)
      

      command <- StatementInterpreter(statementParse, env)
    } yield {
      applyEnvCommand(command)
    }
  }

  // Directly apply an environment comment
  def applyEnvCommand(command: EnvironmentCommand): String = {
    env = env.newCommand(command)
    command.displayString(env)
  }
}

object EnvironmentInterpreter {
  // TODO: Turn this into a file to be read!
  val initialCommands: Vector[String] = Vector(
    "update _default Count.0",
    "update _default Boolean.0",
    "update _default Type.UndefinedType",
    "data String = Array.Char",
    "typeclass _display (t: (t => String))",
    "update _display String.(s: s)"
    
    //"update _default (Array.T).(0.())", // TODO - this can't work yet because it has a pattern!
    //case CustomT("Array", nType) => Success(UCase(UIndex(0), UStruct(Vector.empty)))
      
    // todo - _typeOf should be creatable all in one swoop, and as a generic
    //"update _typeOf Count.(_: Count)",
    //"update _typeOf Identifier.(_: Identifier)",

    //"data Option (T: Type)",
    //"update Option (None, ())",
    //"update Option (Some, T)"
  )
}