package ai.newmap.StateMachine

import ai.newmap.interpreter._
import ai.newmap.model.{EnvStatementParse, ParseElement}

import scala.collection.mutable.ListBuffer

class State (isEndState: Boolean = false, name:String, var tokenStream: Seq[Lexer.Token] = null){
  private var parseElementList: ListBuffer[ParseElement] = ListBuffer()
  private var acceptedTransitions: Seq[Transition] = Seq[Transition]()
  def addAcceptedTransition(transition: Transition): Unit = {
    acceptedTransitions = acceptedTransitions :+ transition
  }

  def reach(p: ListBuffer[ParseElement], ts:Seq[Lexer.Token]): Unit = {
    tokenStream = ts
    parseElementList = p
  }

  def changeState(token: Lexer.Token, tokens: Seq[Lexer.Token]): State = {
    for(transition <- acceptedTransitions){
      if (transition.validateToken(token)){
        return transition.exec(token, parseElementList, tokens)
      }
    }
    new DeadState()
  }

  def getName: String = name
  def endState: Boolean = isEndState
  def generateParseTree: EnvStatementParse = null
}

class DeadState(name:String = "deadState") extends State(name = name)