package controllers

import javax.inject._
import play.api._
import play.api.mvc._
// import play.api.http.HttpEntity
import play.api.libs.json._

import ai.newmap.interpreter._
import ai.newmap.util.{Outcome, Success, Failure}

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def index = Action {
    Ok(views.html.index("Welcome to NewMap.AI!"))
  }

  def processcommand() = Action {
    Ok("Message received! ")
  }

  def slackcommand = Action { request: Request[AnyContent] =>
      val body: AnyContent = request.body
      val msg = body.asFormUrlEncoded.get.get("text").get.head
      val envInt = new EnvironmentInterpreter()
      val response = envInt(msg)
        response match {
          case Success(s) => {
            println(s)
          }
          case Failure(s) => println("Error:\n" + s)
        }
        // visible to everyone
        Ok(Json.obj("response_type" -> "in_channel", "text" -> s"Received: $msg \n Processed: $response"))
  }
}
