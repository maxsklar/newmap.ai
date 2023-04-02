package ai.newmap.interpreter.Parser.StateMachine

import ai.newmap.interpreter.Lexer.Token
import ai.newmap.model.{DisconnectChannelParse, IdentifierParse, ParseElement}

import scala.collection.mutable.ListBuffer

class State (isEndState: Boolean = false){
  private var parseElementList: ListBuffer[ParseElement] = _
  private val acceptedTransitions = List[Transition]()
  def addAcceptedTransition(transition: Transition): Unit = {
    acceptedTransitions :+ transition
  }

  def reach(p: ListBuffer[ParseElement]): Unit = {
    parseElementList = p
  }

  def changeState(token: Token): Unit = {
    for(transition <- acceptedTransitions){
      if (transition.validateToken(token)){
        transition.exec(token, parseElementList)
      }
    }
    println("FAILED TRANSITION")
  }
}

class disconnectChannelEndState extends State(isEndState = true){

  override def reach(p: ListBuffer[ParseElement]): Unit = {
    p.toList
    DisconnectChannelParse(p(1).asInstanceOf[IdentifierParse], p(2).asInstanceOf[IdentifierParse])
  }

}