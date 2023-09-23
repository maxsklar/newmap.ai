package ai.newmap.interpreter

import akka.actor.{Actor, ActorSystem, Props}
import ai.newmap.util.{Success, Failure}

object EnvironmentDaemon {
  val system = ActorSystem("DaemonActorSystem")
  val daemonActor = system.actorOf(Props[DaemonActor], name = "daemonActor")

  case class CodeResponse(
    response: String,
    timeToQuit: Boolean = false
  )

  class DaemonActor extends Actor {
    private val envInterpreter = new EnvironmentInterpreter()

    def receive = {
      case (code: String) => sender() ! passCode(code)
      case (_: Unit) => sender() ! s"Connected to the Environment Daemon"
      case other => sender() ! s"Can't interpret: $other"
    }

    def passCode(code: String): CodeResponse = {
      val response = envInterpreter(code)

      response match {
        case Success(s) => CodeResponse(s, (s == ":exit"))
        case Failure(s) => CodeResponse("Error:\n" + s)
      }
    }
  }
}
