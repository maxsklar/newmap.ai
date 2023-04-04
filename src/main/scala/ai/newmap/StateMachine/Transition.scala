package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer
import ai.newmap.model.ParseElement

import scala.collection.mutable.ListBuffer

class Transition (token: Lexer.Token, nextState: State){

  def validateToken(t: Lexer.Token): Boolean = {
    t == token
  }

  def exec(t: Lexer.Token, partialParseElementList: ListBuffer[ParseElement]): State = {
    partialParseElementList += t
    nextState.reach(partialParseElementList)
    nextState
  }
}