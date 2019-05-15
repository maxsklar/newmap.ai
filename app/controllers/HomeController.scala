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
import ai.newmap.nluLayer.nluInterpreter.preProcess
import ai.newmap.nluLayer.convTerminator.stopConv
import ai.newmap.nluLayer.nluChecker.GoingToGet
import ai.newmap.nluLayer.nluChecker.Got
import ai.newmap.nluLayer.nluInterpreter.generateRegularJsonRespond
import ai.newmap.nluLayer.nluInterpreter.retJsonFormatFlag
import ai.newmap.nluLayer.nluInterpreter.dummyGreetingResp
import ai.newmap.nluLayer.onBoardConstant.ActRecommendation
import ai.newmap.nluLayer.onBoardConstant.GreetingConstant
import ai.newmap.logger.adminLogger
import scala.collection.immutable.HashSet
import ai.newmap.environment.envCommiter.getNextNameableChar

import play.api.libs.json._

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

  var randomKey = ""
	
  def newmap = Action { request: Request[AnyContent] =>
  	val body: AnyContent = request.body
  	val msg = body.asFormUrlEncoded.get.get("text").get.head

    // channel name:
    val chanName = body.asFormUrlEncoded.get.get("channel_name").get.head
    // user name:
    val userName = body.asFormUrlEncoded.get.get("user_name").get.head
    //Ok(chanName+" "+userName)
    //Ok(">> "+msg)

    val AuthorizedUser:HashSet[String] = HashSet("max.sklar", "yw2983", "na2196", "yg1702")

    if(msg.equals("admin print")){
      if(AuthorizedUser.contains(userName)){
        val adminEndpoint = "https://newmap-ai.herokuapp.com/adminPrint?code="
        val res = new StringBuilder
        val r = scala.util.Random
        for(c <- for (i <- 1 to 4) yield getNextNameableChar(r)) {
          res += c
        }
        randomKey = res.toString
        Ok(">> admin print\n"+adminEndpoint+randomKey)
      }else{
        Ok("*You are not authorized to see the admin log*")
      }
    }else{
      adminLogger.log(chanName, userName, ">> "+msg)
      if(msg.equals("train")){
        if(AuthorizedUser.contains(userName)) {
          train
          adminLogger.log(chanName, userName, "*Train Finished*")
          Ok("*Train Finished*")
        }else{
          adminLogger.log(chanName, userName, "*You are not authorized to train the model*")
          Ok("*You are not authorized to train the model*")
        }
      }else if(msg.toLowerCase.equals("help")){
        adminLogger.log(chanName, userName, ActRecommendation)
        Ok(">> "+msg+"\n"+ActRecommendation)
      }else if(msg.equals("stop")){
        stopConv(chanName, userName)
        adminLogger.log(chanName, userName, "Conversation Stopped")
        Ok(">> "+msg+"\n"+"Conversation Stopped")
      }else{
        val checkGreeting = dummyGreetingResp(msg)
        checkGreeting match {
          case "N" => {
            val ret = nluInterp(chanName, userName, msg)
            if(retJsonFormatFlag){
              Ok(Json.parse(ret))
            }else{
              Ok(ret)
            }
          }
          case _ => {
            adminLogger.log(chanName, userName, GreetingConstant)
            Ok(">> "+msg+"\n"+checkGreeting)
          }
        }
      }
    }
  }

  def handle_button_request = Action { request: Request[AnyContent] =>
    val resp = request.body.asFormUrlEncoded.get.get("payload").get.head.split(",")(3).replaceAll("\"", "").stripSuffix("}]").split(":")(1)
    // channel name:
    val chanName = request.body.asFormUrlEncoded.get.get("payload").get.head.split(",")(8).replaceAll("\"", "").stripSuffix("}").split(":")(1)
    // user name:
    val userName = request.body.asFormUrlEncoded.get.get("payload").get.head.split(",")(10).replaceAll("\"", "").stripSuffix("}").split(":")(1)
    resp match {
      case "No" => {
        adminLogger.log(chanName, userName, ">> Button(No)")
        stopConv(chanName, userName)
        adminLogger.log(chanName, userName, "*Conversation stopped!*\n*Please type 'Help' for more instructions*")
        Ok("*Conversation stopped!*\n*Please type 'Help' for more instructions*")
        //Ok(request.body.asFormUrlEncoded.get.get("payload").get.head)
      }
      case _ => {
        adminLogger.log(chanName, userName, ">> Button(Yes)")
        adminLogger.log(chanName, userName, GoingToGet)
        Ok(Json.parse(generateRegularJsonRespond(Got+"""\n"""+GoingToGet)))
      }
    }
    // Ok(request.body.asFormUrlEncoded.get.get("payload").get.head.split(",")(3).replaceAll("\"", "").stripSuffix("}]").split(":")(1)+"")
  }

  def test_get(code : String) = Action {
    // channel name:
    val chanName = "testChanName"
    // user name:
    val userName = "admin"
    if(code.equals("train")){
      train
      Ok("Train Finished")
    }else if(code.equals("stop")){
      stopConv(chanName, userName)
      Ok("Conversation Stopped")
    }else if(code.equals("adminLog")){
      adminLogger.adminPrint
      Ok("Admin Log")
    }else{
      val ret = nluInterp(chanName, userName, code)
      Ok(ret)
    }
  }

  def admin_print(code: String) = Action {
    if(code.equals(randomKey)) {
      val ret = adminLogger.adminPrint
      Ok("Administor LOG: \n"+ret)
    }else{
      Ok("Deprecated Endpoint \nPlease try 'admin print' again on slack")
    }
  }

  def newmap_script = Action { request: Request[AnyContent] =>
    val body: AnyContent = request.body
    val code = body.asFormUrlEncoded.get.get("text").get.head

    // channel name:
    val chanName = body.asFormUrlEncoded.get.get("channel_name").get.head
    // user name:
    val userName = body.asFormUrlEncoded.get.get("user_name").get.head

    var response:String = ""
    var envInterp = new EnvironmentInterpreter()
    envInterp.setChanName(chanName)
    envInterp.setUserName(userName)

    if(code.equals("admin print")){
      val AuthorizedUser:HashSet[String] = HashSet("max.sklar", "yw2983", "na2196", "yg1702")
      if(AuthorizedUser.contains(userName)){
        val adminEndpoint = "https://newmap-ai.herokuapp.com/adminPrint?code="
        val res = new StringBuilder
        val r = scala.util.Random
        for(c <- for (i <- 1 to 4) yield getNextNameableChar(r)) {
          res += c
        }
        randomKey = res.toString
        Ok(">```> admin print```\n>```< "+adminEndpoint+randomKey+"```")
      }else{
        Ok("You are not authorized to see the admin log")
      }

    }else{
      code match {
        case code if (code.startsWith(":create ") ||
                      code.startsWith(":log in ") ||
                      code.startsWith(":copy ") ||
                      code.startsWith(":comment on") ||
                      code.startsWith(":commit") ||
                      code.startsWith(":checkout ") ||
                      code.startsWith(":reset ") ||
                      code.startsWith(":hard set ")) => {
          response = prettyPrinter(""+envInterp(code))
        }
        case ":printEnv" => {
          response = prettyPrinter(""+envInterp(code))
        }
        case ":envs" => {
          response = prettyPrinter(""+envInterp(code))
        }
        case ":log off" => {
          response = prettyPrinter(""+envInterp(code))
        }
        case ":printLog" =>{
          response = prettyPrinter(""+envInterp(code))
        }
        case ":help" =>{
          response = prettyPrinter(""+envInterp(code))
        }
        case _ =>{
          response = ""+envRead(chanName, userName, code)
        }
      }
      Ok(">```newmap script lang > "+code+"```\n>```newmap script lang < "+response.replaceAll("""[*]""", "").trim+"```")
    }

  }





  // deprecated
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
                    code.startsWith(":hard set "))=>{
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

}
