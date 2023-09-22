package ai.newmap.interpreter

import ai.newmap.util.{Success, Failure}
import org.jline.reader.impl.history.DefaultHistory
import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.TerminalBuilder
import akka.util.Timeout
import akka.pattern.ask
import java.nio.file.Paths
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

/**
 * This is the main class that opens a repl to the appropriate environment daemon.
*/
object repl extends App {
  implicit val timeout: Timeout = 5.seconds

  val historyFile = Paths.get(System.getProperty("user.home"), ".newmap_repl_history")

  val terminal = TerminalBuilder.builder().build()
  
  val lineReader = LineReaderBuilder.builder()
    .terminal(terminal)
    .variable(LineReader.HISTORY_FILE, historyFile)
    .build()

  val history = lineReader.getHistory.asInstanceOf[DefaultHistory]
  history.attach(lineReader)
  history.load()

  // This ensures that the environment daemon is initialized
  val unit: Unit = ()
  val pingResponseF = (EnvironmentDaemon.daemonActor ? unit)
  val pingResponse = Await.result(pingResponseF, 5.seconds)
  println(pingResponse)
  
  var continue = true
  while(continue) {
    val code = lineReader.readLine("> ")

    val responseF = EnvironmentDaemon.daemonActor ? code

    val response = Await.result(responseF, 5.seconds)

    response match {
      case (s: EnvironmentDaemon.CodeResponse) =>
        if (s.timeToQuit) {
          history.save()
          continue = false
        } else {
          if (s.response.nonEmpty) println(s.response)
        }
      case _ => println(response.toString)
    }
  }

  EnvironmentDaemon.system.terminate()
}