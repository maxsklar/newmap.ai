package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

   def newmap_script = Action { request: Request[AnyContent] =>
    val body: AnyContent = request.body
    val script = body.asFormUrlEncoded.get.get("text").get.head
    var envInterp = new EnvironmentInterpreter()
    var c : String = ""
    var res :String = ""
    for( c <-script.split(";") ){
      res += "Input: "+c.replaceAll("^\\s+", "")+"; Output: "+envInterp(c)+" \n"
    }
    Ok(""+res)
  }

  def newmap_script_get(code : String) = Action {
    var envInterp = new EnvironmentInterpreter()
    var c : String = ""
    var res :String = ""
    for( c <-code.split(";") ){
      res += "Input: "+c.replaceAll("^\\s+", "")+"; Output: "+envInterp(c)+" \n"
    }
    Ok(""+res)
  }

}