package controllers

import javax.inject._
import play.api._
import play.api.mvc._

import ai.newmap.model._
import ai.newmap.interpreter._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}
import ai.newmap.environment.envReader.envRead
import ai.newmap.environment.envReader.envLogIn
import ai.newmap.environment.envCreater.envCreate

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
    Ok(views.html.index())
  }
	
  def newmap = Action { request: Request[AnyContent] =>
  	val body: AnyContent = request.body
  	val msg = body.asFormUrlEncoded.get.get("text").get.head

    // channel name:
    val chanName = body.asFormUrlEncoded.get.get("channel_name").get.head
    // user name:
    val userName = body.asFormUrlEncoded.get.get("user_name").get.head
    //Ok(chanName+" "+userName)
    var response:String = ""
    msg match{
      case msg if msg.startsWith(":create ")=>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = ""+envInterp(msg)
      }
      case msg if msg.startsWith(":log in ")=>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = ""+envInterp(msg)
      }
      case ":env" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = ""+envInterp(msg)
      }
      case _ =>{
        response = ""+envRead(chanName, userName, msg)
      }
    }
    Ok("Input: "+msg+" \n"+response)
  }

  def newmap_get(code : String) = Action {
    // channel name:
    val chanName = "testChanName"
    // user name:
    val userName = "admin"
    //var envInterp = new EnvironmentInterpreter()
    var response:String = ""
    code match{
      case code if code.startsWith(":create ")=>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = ""+envInterp(code)
      }
    case code if code.startsWith(":log in ")=>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = ""+envInterp(code)
    }
    case ":env" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = ""+envInterp(code)
    }
    case _ =>{
        response = ""+envRead(chanName, userName, code)
      }
    }
    Ok(""+response)
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
