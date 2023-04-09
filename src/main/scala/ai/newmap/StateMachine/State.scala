package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer.Token
import ai.newmap.model.ParseElement

import scala.collection.mutable.ListBuffer

class State (isEndState: Boolean = false, name:String){
  private var parseElementList: ListBuffer[ParseElement] = ListBuffer()
  private var acceptedTransitions: Seq[Transition] = Seq[Transition]()
  def addAcceptedTransition(transition: Transition): Unit = {
    acceptedTransitions = acceptedTransitions :+ transition
  }

  def reach(p: ListBuffer[ParseElement]): Unit = {
    println("Reached " + name)
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
}

class DeadState(name:String = "deadState") extends State(name = name)