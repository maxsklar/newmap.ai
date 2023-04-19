package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer.Token
import ai.newmap.model.{EnvStatementParse, ParseElement}

import scala.collection.mutable.ListBuffer

class State (isEndState: Boolean = false, name:String){
  private var parseElementList: ListBuffer[ParseElement] = ListBuffer()
  private var acceptedTransitions: Seq[Transition] = Seq[Transition]()
  def addAcceptedTransition(transition: Transition): Unit = {
    acceptedTransitions = acceptedTransitions :+ transition
  }

  def reach(p: ListBuffer[ParseElement]): Unit = {
    parseElementList = p
  }

  def changeState(token: Token): State = {
    for(transition <- acceptedTransitions){
      if (transition.validateToken(token)){
        return transition.exec(token, parseElementList)
      }
    }
    new DeadState()
  }

  def getName: String = name
  def endState: Boolean = isEndState
  def generateParseTree: EnvStatementParse = null
}

class DeadState(name:String = "deadState") extends State(name = name)