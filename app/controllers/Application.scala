package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def slackcommand(msg : String) = Action {
    // Sending JSON
    Ok("slack command in development")
    Ok(s"processing $msg")
  }
}
