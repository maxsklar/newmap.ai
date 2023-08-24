package ai.newmap.interpreter

import ai.newmap.util.{Success, Failure}
import org.jline.reader.{LineReaderBuilder, UserInterruptException, EndOfFileException}
import org.jline.terminal.TerminalBuilder

// TODO - create a custom shell
object repl {
  private val envInterpreter = new EnvironmentInterpreter()
  val terminal = TerminalBuilder.builder().build()
  val lineReader = LineReaderBuilder.builder().terminal(terminal).build()

  def main(args: Array[String]): Unit = {
    var continue = true
    while(continue) {
      val code = lineReader.readLine("> ")
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