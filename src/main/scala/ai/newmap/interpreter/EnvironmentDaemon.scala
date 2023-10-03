package ai.newmap.interpreter

import akka.actor.{Actor, ActorRef, ActorSystem, ActorIdentity, ExtendedActorSystem, Identify, Props}
import akka.pattern.after
import akka.util.Timeout
import ai.newmap.util.{Success, Failure}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import java.net.Socket
import java.net.InetSocketAddress
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * This handles either creating an environment daemon, or connecting to 
 *  an existing daemon on another port.
 */
class EnvironmentDaemon(portOpt: Option[Int]) {
  implicit val timeout: Timeout = 5.seconds

  def isPortOpen(host: String, port: Int, timeoutMillis: Int = 2000): Boolean = {
    val socket = new Socket()
    try {
      socket.connect(new InetSocketAddress(host, port), timeoutMillis)
      true
    } catch {
      case _: Throwable => false
    } finally {
      try {
        socket.close()
      } catch {
        case _: Throwable => // Ignore
      }
    }
  }

  val unit: Unit = ()

  // This ensures that the environment daemon is initialized
  val resultF: Future[(ActorRef, ActorSystem)] = {
    val actorSystem: ActorSystem = ActorSystem("DaemonActorSystem")
    for {
      result: ActorRef <- portOpt match {
        case None => {
          val actorRef = actorSystem.actorOf(Props[DaemonActor], name = "daemonActor")
          
          for {
            pingResponse <- actorRef ? unit
          } yield {
            println(pingResponse)
            actorRef
          }
        }
        case Some(port) if !isPortOpen("localhost", port) => {
          println(s"port $port not open, creating a new EnvironemntDaemon")
          val actorRef = actorSystem.actorOf(Props[DaemonActor], name = "daemonActor")
          
          for {
            pingResponse <- actorRef ? unit
          } yield {
            println(pingResponse)
            actorRef
          }
        }
        case Some(port) => {
          val daemonActorPath = s"akka://DaemonActorSystem@127.0.0.1:$port/user/daemonActor"
          println(s"Connecting to path $daemonActorPath")
          val selection = actorSystem.actorSelection(daemonActorPath)

          for {
            pingResponse <- (selection ? unit)
            actorF = (selection ? Identify(None)).mapTo[ActorIdentity]
            actor <- actorF
          } yield {
            println(pingResponse)
            actor.ref.get
          }
        }
      }
    } yield (result -> actorSystem)
  }

  val result = Await.result(resultF, 5.seconds)

  val (daemon, actorSystemRef) = result

  def terminate(): Unit = actorSystemRef.terminate()

  def sendCode(code: String): Future[Any] = daemon ? code
}