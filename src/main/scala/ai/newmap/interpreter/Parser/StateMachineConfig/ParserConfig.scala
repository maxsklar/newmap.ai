package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, StateMachine}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Parser.StateMachineConfig.DisconnectChannelPath.disconnectChannelInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.IteratePath.iterateInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.ConnectChannelPath.connectChannelInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.ForkChannelPath.forkedVersionedStmtInitTransition
//import ai.newmap.interpreter.Parser.StateMachineConfig.applyCommandPath.applyCommandStmtInitTransition
import ai.newmap.util.Outcome

class ParserConfig() {

  val initState = new State(name = "INIT" )
  initState.addAcceptedTransition(disconnectChannelInitTransition)
  initState.addAcceptedTransition(iterateInitTransition)
  initState.addAcceptedTransition(connectChannelInitTransition)
  initState.addAcceptedTransition(forkedVersionedStmtInitTransition)
//  initState.addAcceptedTransition(applyCommandStmtInitTransition)
}

object StateMachineRunner{
  def run(tokens: Seq[Lexer.Token]): Outcome[Any, String] = {
    val sm = new StateMachine()
    sm.run(tokens)
  }
}