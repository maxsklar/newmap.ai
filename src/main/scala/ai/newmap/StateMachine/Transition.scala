package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer
import ai.newmap.model.ParseElement

import scala.collection.mutable.ListBuffer

class Transition (expectedToken: Lexer.Token = null, expectedClass:Class[_] = null, nextState: State){

  def validateToken(t: Lexer.Token): Boolean = {
    t == expectedToken || expectedClass == t.getClass
  }

  def exec(t: Lexer.Token, partialParseElementList: ListBuffer[ParseElement]): State = {
    if((expectedClass != null && expectedClass == t.getClass) || t == expectedToken){
      partialParseElementList += t
      nextState.reach(partialParseElementList)
      nextState
    }
    else{
      new DeadState()
    }
  }
}