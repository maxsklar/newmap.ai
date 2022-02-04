package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}

// TODO - create a custom shell
object Repl {
  var envInterp = new EnvironmentInterpreter()

  def main(args: Array[String]): Unit = {
    var continue = true
    while(continue) {
      print("> ")
      val code = scala.io.StdIn.readLine()
      val response = envInterp(code)
      response match {
        case Success(s) => {
          if (s == ":exit") {
            continue = false
          } else {
            println(s)
          }
        }
        case Failure(s) => println("Error:\n" + s)
      }
    }
  }
}