package ai.newmap.interpreter.parser.stateMachine

import ai.newmap.interpreter.Lexer
import ai.newmap.model.EnvStatementParse
import ai.newmap.util.{Failure, Success, Outcome}
import ai.newmap.interpreter.parser.stateMachineConfig.InitStatementState

case class StateMachine[OutT](initState: ParseState[OutT]) {
  def run(
    tokens: Seq[Lexer.Token],
    curState: ParseState[OutT] = initState
  ): Outcome[OutT, String] = tokens match {
    case firstToken +: otherTokens => {
      //println("current state: " + curState)
      for {
        newState <- curState.update(firstToken)
        result <- run(otherTokens, newState)
      } yield result
    }
    case Nil => {
      //println("end state: " + curState)
      Outcome(curState.generateOutput, "Unimplemented")
    }
  }
}