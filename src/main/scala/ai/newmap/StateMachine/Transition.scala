package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer
import ai.newmap.model.ParseElement
import ai.newmap.util.{Success,Failure}
import ai.newmap.model.ParseTree

import scala.collection.mutable.ListBuffer

class Transition(
  tokenValidator: Lexer.Token => Boolean = TokenValidators.none,
  nextState: State,
  nextExpectedParseTree: Class[_] = null,
  var tokenStream: Seq[Lexer.Token] = Seq.empty
) {
  def validateToken(t: Lexer.Token): Boolean = tokenValidator(t)

  def exec(t: Lexer.Token, partialParseElementList: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): State = {
    if (nextExpectedParseTree == null && nextState != null) {
      execStateTransition(t, partialParseElementList, ts)
    }
    else if (nextExpectedParseTree != null && nextState != null){
      val sm = new StateMachine(1, nextExpectedParseTree)
      val outcome = sm.run(tokenStream)
      outcome match {
        case Success(v) => {
          partialParseElementList += v.asInstanceOf[ParseElement]
          execStateTransition(null, partialParseElementList, ts)
        }
        case Failure(_) => new DeadState()
      }
    }
    else new DeadState()
  }

  private def execStateTransition(t: Lexer.Token, partialParseElementList: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): State = {
    if (tokenValidator(t)) {
      partialParseElementList += t
      tokenStream = ts
      nextState.reach(partialParseElementList, tokenStream)
      nextState
    }
    else new DeadState()
  }
}