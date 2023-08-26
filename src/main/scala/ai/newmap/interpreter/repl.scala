package ai.newmap.interpreter

import ai.newmap.util.{Success, Failure}
import org.jline.reader.{LineReaderBuilder, UserInterruptException, EndOfFileException}
import org.jline.terminal.TerminalBuilder
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

// TODO - create a custom shell
object repl {
  private val envInterpreter = new EnvironmentInterpreter()
  val terminal = TerminalBuilder.builder().build()
  val lineReader = LineReaderBuilder.builder().terminal(terminal).build()

  implicit val timeout: Timeout = 5.seconds

  def main(args: Array[String]): Unit = {
    var continue = true
    while(continue) {
      val code = lineReader.readLine("> ")

      val responseF = EnvironmentDaemon.daemonActor ? code

      val response = Await.result(responseF, 5.seconds)

      response match {
        case (s: EnvironmentDaemon.CodeResponse) =>
          if (s.timeToQuit) {
            continue = false
          } else {
            if (s.response.nonEmpty) println(s.response)
          }
        case _ => println(response.toString)
      }
    }

    EnvironmentDaemon.system.terminate()
  }
}