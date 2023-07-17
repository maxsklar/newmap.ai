package ai.newmap.StateMachine

import ai.newmap.interpreter._
import ai.newmap.model.{EnvStatementParse, ParseElement}

import scala.collection.mutable.ListBuffer

case class State(
  val name: String,
  val isEndState: Boolean = false
) {
  private var parseElementList: ListBuffer[ParseElement] = ListBuffer()
  private var acceptedTransitions: Seq[Transition] = Seq[Transition]()
  
  def addAcceptedTransition(transition: Transition): Unit = {
    acceptedTransitions = acceptedTransitions :+ transition
  }

  def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): Unit = {
    parseElementList = p
  }

  def changeState(token: Lexer.Token, tokens: Seq[Lexer.Token]): State = {
    for(transition <- acceptedTransitions) {
      if (transition.tokenValidator(token)) {
        return transition.exec(token, parseElementList, tokens)
      }
    }

    State.Dead
  }

  def generateParseTree: EnvStatementParse = null
}

object State {
  val Dead = State("deadState")
}