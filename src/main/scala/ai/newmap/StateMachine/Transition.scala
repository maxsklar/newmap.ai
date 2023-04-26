package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer
import ai.newmap.model.ParseElement

import scala.collection.mutable.ListBuffer

class Transition (expectedToken: Lexer.Token = null,
                  expectedTokenClass:Class[_] = null,
                  nextState: State,
                  nextExpectedParseTree: Class[_] = null,
                  var tokenStream: Seq[Lexer.Token] = null){
  def validateToken(t: Lexer.Token): Boolean = {
    t == expectedToken || expectedTokenClass == t.getClass
  }

  def exec(t: Lexer.Token, partialParseElementList: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): State = {
    if(nextExpectedParseTree == null && nextState != null){
      execStateTransition(t, partialParseElementList, ts)
    }
    else if(nextExpectedParseTree != null && nextState != null){
      val sm = new StateMachine(0, nextExpectedParseTree)
      val outcome= sm.run(tokenStream)
      outcome match {
        case Success(v) =>
          partialParseElementList += v.asInstanceOf[ParseElement]
          execStateTransition(null, partialParseElementList, ts)
        case Failure(_) =>
          new DeadState()
      }
    }
    else{
      new DeadState()
    }
  }

  private def execStateTransition(t: Lexer.Token, partialParseElementList: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): State = {
    if ((expectedTokenClass != null && expectedTokenClass == t.getClass) || t == expectedToken) {
      partialParseElementList += t
      tokenStream = ts
      nextState.reach(partialParseElementList, tokenStream)
      nextState
    }
    else {
      new DeadState()
    }
  }
}