package ai.newmap.interpreter

import org.jline.reader.impl.history.DefaultHistory
import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.TerminalBuilder
import akka.util.Timeout
import akka.actor.{Actor, ActorRef, ActorSystem, ActorIdentity, ExtendedActorSystem, Identify, Props}
import java.nio.file.Paths
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import java.net.Socket
import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext.Implicits.global
import com.typesafe.config.ConfigFactory

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

  def getPortArg(args: Array[String]): Option[Int] = {
    args.sliding(2, 1).collectFirst {
      case Array("--port", port: String) if port.forall(_.isDigit) => port.toInt
    }
  }

  val envDaemon = new EnvironmentDaemon(getPortArg(args))
  
  var continue = true
  while(continue) {
    val code = lineReader.readLine("> ")

    val responseF = envDaemon.sendCode(code)

    val response = Await.result(responseF, 5.seconds)

    response match {
      case (s: DaemonActor.CodeResponse) =>
        if (s.timeToQuit) {
          history.save()
          continue = false
        } else {
          if (s.response.nonEmpty) println(s.response)
        }
      case _ => println(response.toString)
    }
  }

  envDaemon.terminate()
}