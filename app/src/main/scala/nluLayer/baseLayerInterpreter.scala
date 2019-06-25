package ai.newmap.nluLayer

import ai.newmap.model._
import ai.newmap.interpreter._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}
import ai.newmap.environment.envReader.envRead
import ai.newmap.environment.envReader.envLogIn
import ai.newmap.environment.envCreater.envCreate
import ai.newmap.environment.envPrinter.prettyPrinter

object baseLayerInterpreter {

	def interp(chanName: String, userName: String, msg: String): String = {
		var response:String = ""
		msg match{
	      case msg if (msg.startsWith(":create ") ||
	                    msg.startsWith(":log in ") ||
	                    msg.startsWith(":copy ") ||
	                    msg.startsWith(":comment on")||
	                    msg.startsWith(":commit") ||
	                    msg.startsWith(":checkout ") ||
	                    msg.startsWith(":reset ") ||
	                    msg.startsWith(":hard reset "))=>{
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
	    response
	}
}
