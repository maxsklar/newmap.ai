package ai.newmap.interpreter

import akka.actor.{Actor, ActorSystem, ExtendedActorSystem, Props}
import akka.pattern.after
import ai.newmap.util.{Success, Failure}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class DaemonActor extends Actor {
  private val envInterpreter = new EnvironmentInterpreter()

  def getBoundPort(): Future[Int] = {
    implicit val actorSystem: ActorSystem = context.system
    context.system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress.port match {
      case Some(port) => Future.successful(port)
      case None => {
        after(100.millis, actorSystem.scheduler) {
          getBoundPort()
        }
      }
    }
  }

  def receive = {
    case (code: String) => sender() ! passCode(code)
    case (_: Unit) => sender() ! {
      val boundPort = Await.result(getBoundPort(), 400.millis)
      //val boundPortStr = boundPort.toOption.map(_.toString).getOrElse("not avilable")
      s"Connected to the Environment Daemon on port $boundPort at path ${self.path}"
    }
    case other => sender() ! s"Can't interpret: $other"
  }

  def passCode(code: String): DaemonActor.CodeResponse = {
    val response = envInterpreter(code)

    response match {
      case Success(s) => DaemonActor.CodeResponse(s, (s == ":exit"))
      case Failure(s) => DaemonActor.CodeResponse("Error:\n" + s)
    }
  }
}

object DaemonActor {
  case class CodeResponse(
    response: String,
    timeToQuit: Boolean = false
  )
}