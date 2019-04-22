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
import ai.newmap.environment.envPrinter.prettyPrinter

import ai.newmap.nluLayer.trainer.train
import ai.newmap.nluLayer.nluInterpreter.nluInterp
import ai.newmap.nluLayer.nluInterpreter.loadModel

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
      case msg if (msg.startsWith(":create ") ||
                    msg.startsWith(":log in ") ||
                    msg.startsWith(":copy ") ||
                    msg.startsWith(":comment on")||
                    msg.startsWith(":commit") ||
                    msg.startsWith(":checkout ") ||
                    msg.startsWith(":reset ") ||
                    msg.startsWith(":hard reset ") ||
                    msg.startsWith(":script"))=>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(msg))
      }
      case ":printEnv" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(msg))
      }
      case ":envs" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(msg))
      }
      case ":log off" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(msg))
      }
      case ":printLog" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(msg))
      }
      case ":help" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(msg))
      }
      case _ =>{
        response = ""+envRead(chanName, userName, msg)
      }
    }
    Ok(">> "+msg+" \n"+response)
  }

  def newmap_get(code : String) = Action {
    // channel name:
    val chanName = "testChanName"
    // user name:
    val userName = "admin"
    //var envInterp = new EnvironmentInterpreter()
    var response:String = ""
    code match{
      case code if (code.startsWith(":create ") ||
                    code.startsWith(":log in ") ||
                    code.startsWith(":copy ") ||
                    code.startsWith(":comment on") ||
                    code.startsWith(":commit") ||
                    code.startsWith(":checkout ") ||
                    code.startsWith(":reset ") ||
                    code.startsWith(":hard set ") ||
                    code.startsWith(":script"))=>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(code))
      }
    case ":printEnv" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(code))
    }
    case ":envs" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(code))
    }
    case ":log off" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(code))
    }
    case ":printLog" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(code))
    }
    case ":help" =>{
        var envInterp = new EnvironmentInterpreter()
        envInterp.setChanName(chanName)
        envInterp.setUserName(userName)
        response = prettyPrinter(""+envInterp(code))
    }
    case _ =>{
        response = ""+envRead(chanName, userName, code)
      }
    }
    Ok(">> "+code+" \n"+response)
  }



  def test_get(code : String) = Action {
    // channel name:
    val chanName = "testChanName"
    // user name:
    val userName = "admin"
    if(code.equals("train")){
      train
      Ok("train finished")
    }elseelse if(code.equals("load")){
      loadModel
      Ok("model loaded")
    }else{
      Ok("failed")
    }
  }

  def newmap_script = Action { request: Request[AnyContent] =>
    val body: AnyContent = request.body
    val script = body.asFormUrlEncoded.get.get("text").get.head