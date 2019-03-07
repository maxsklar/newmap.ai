package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}
import ai.newmap.environment.envCreater.envCreate
import ai.newmap.environment.envReader.envLogIn
import ai.newmap.environment.envPrinter.envPrint

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

  def createEnv(chanName: String, envName: String, envAccessCode: String): CommandInterpResponse = {
    val ret: Boolean = envCreate(chanName, envName, envAccessCode)
    if(ret)CommandPrintSomething("created environment")
    else{
      CommandPrintSomething("env already exits")
    }    
  }

<<<<<<< HEAD
  def logInEnv(chanName: String, envName: String, envAccessCode: String): CommandInterpResponse = {
    val ret: Boolean = envLogIn(chanName, envName, envAccessCode)
    if(ret)CommandPrintSomething("finish import")
=======
  def logInEnv(envName: String, envAccessCode: String): CommandInterpResponse = {
    val ret: Boolean = envLogIn(this.chanName, this.userName, envName, envAccessCode)
<<<<<<< HEAD
    if(ret)CommandPrintSomething("loged into "+envName)
>>>>>>> e183478... add :help about create and log in
=======
    if(ret)CommandPrintSomething("loged into Environment "+envName)
>>>>>>> 1a42f2f... solve AWS security issue
    else{
      CommandPrintSomething("Could not log in")
    }
  }  

  def applyInterpCommand(code: String): CommandInterpResponse = {
    code match {
      case ":create" => this.createEnv("newmap_test", "Workspace", "aaa")
      case code if code.startsWith(":log in ") => {
                        val cont:Array[String] = code.stripPrefix(":log in ").split("\\s+")
                        this.logInEnv(cont(0), cont(1), cont(2))
                      }
      //case ":env" => CommandPrintSomething(env.toString)
      case ":env" => CommandPrintSomething(envPrint(this.chanName, this.userName))
      case ":exit" | ":quit" => CommandExit
      case ":help" => CommandPrintSomething(
        "List of environment commands\n" ++
        ":env\tPrint the current environment\n" ++
        //":exit | :quit\tExit this repl\n" ++
        ":create <env name> <env password>\tCreate a new environment\n" ++
        ":log in <env name> <env password>\tLog in to an exist environment\n" ++
        ":help\tPrint this help message\n"
      )
      case _ => CommandPassThrough
    }
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
  val initialCommands: Vector[String] = Vector(
    "val MutableDescriptor: Type = Struct(versionType: (Count => Type), init: (versionType 0), commandType: Type, update: ((version: Count, current: versionType version, command: commandType) => versionType (increment version)))",
    "val CounterV: MutableDescriptor = (versionType: ((n: Count) => Count), init: 0, commandType: Struct(), update: ((version: Count, current: versionType version, command: commandType) => increment current))",
    "val StackV: ((T: Type, default: T) => MutableDescriptor) = (T: Type, default: T) => (versionType: ((n: Count) => T), init: default, commandType: T, update: ((version: Count, current: versionType version, command: commandType) => command))",
    "val SequenceV: ((T: Type, default: T) => MutableDescriptor) = (T: Type, default: T) => (versionType: ((n: Count) => Map n T default), init: (), commandType: T, update: ((version: Count, current: versionType version, command: commandType) => appendSeq version commandType default current command))",
    "val MapV: ((keyType: Type, valueType: Type, default: valueType) => MutableDescriptor) = (keyType: Type, valueType: Type, default: valueType) => (versionType: ((n: Count) => Map keyType valueType default), init: (), commandType: Map keyType valueType default, update: ((version: Count, current: versionType version, command: commandType) => appendMap keyType valueType default current command))"
  )
}