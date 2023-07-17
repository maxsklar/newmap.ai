package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer
import ai.newmap.model.ParseElement
import ai.newmap.util.{Success,Failure}
import ai.newmap.model.ParseTree

import scala.collection.mutable.ListBuffer

case class Transition(
  val tokenValidator: Lexer.Token => Boolean = TokenValidators.none,
  val nextState: State,
  val expectingParseTree: Boolean = false
) {
  def exec(t: Lexer.Token, partialParseElementList: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): State = {
    if (!expectingParseTree) {
      execStateTransition(t, partialParseElementList, ts)
    }
    else {
      // TODO - work this part out
      // NOTE(max): it appears to have to do with looking for a sub-expression within a parse.
      val sm = new StateMachine(1)
      val outcome = sm.run(Seq.empty)
      outcome match {
        case Success(v) => {
          partialParseElementList += v.asInstanceOf[ParseElement]
          execStateTransition(Lexer.EndToken, partialParseElementList, ts)
        }
        case Failure(_) => State.Dead
      }
    }
  }

  private def execStateTransition(t: Lexer.Token, partialParseElementList: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): State = {
    if (tokenValidator(t)) {
      partialParseElementList += t
      nextState.reach(partialParseElementList, ts)
      nextState
    }
    else State.Dead
  }
}