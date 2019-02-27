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

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok("Welcome to NewMap.AI!")
    // Ok(views.html.index())
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
        Ok(s"Processed: $response")
  }
}
