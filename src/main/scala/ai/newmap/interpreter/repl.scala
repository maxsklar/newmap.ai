package ai.newmap.interpreter

import ai.newmap.util.{Success, Failure}

// TODO - create a custom shell
object repl {
  private val envInterpreter = new EnvironmentInterpreter()

  def main(args: Array[String]): Unit = {
    var continue = true
    while(continue) {
      print("> ")
      val code = scala.io.StdIn.readLine()
      val response = envInterpreter(code)
      response match {
        case Success(s) =>
          if (s == ":exit") {
            continue = false
          } else {
            if (s.nonEmpty) println(s)
          }
        case Failure(s) => println("Error:\n" + s)
      }
    }
  }
}