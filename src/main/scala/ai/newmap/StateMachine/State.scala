package ai.newmap.StateMachine

import ai.newmap.interpreter._
import ai.newmap.model.{EnvStatementParse, ParseElement}

import scala.collection.mutable.ListBuffer

case class State(
  val name: String,
  val transitions: Seq[Transition] = Seq.empty,
  val isEndState: Boolean = false
) {
  private var parseElementList: ListBuffer[ParseElement] = ListBuffer()

  def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): Unit = {
    parseElementList = p
  }

  def nextState(token: Lexer.Token, tokens: Seq[Lexer.Token]): State = {
    transitions.find(_.tokenValidator(token)).map(_.exec(token, parseElementList, tokens)).getOrElse(State.Dead)
  }

  def generateParseTree: Option[EnvStatementParse] = None
}

object State {
  val Dead = State("deadState")
}