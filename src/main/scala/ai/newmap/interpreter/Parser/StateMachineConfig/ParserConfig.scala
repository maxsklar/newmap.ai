package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, StateMachine}
import ai.newmap.interpreter.Lexer.Token
import ai.newmap.model.EnvStatementParse
import ai.newmap.interpreter.Parser.StateMachineConfig.DisconnectChannelPath.disconnectChannelInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.IteratePath.iterateInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.ConnectChannelPath.connectChannelInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.DataPath.dataInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.ForkChannelPath.forkedVersionedStmtInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.WritePath.writeInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.applyCommandPath.applyCommandStmtInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.applyCommandsPath.applyCommandsStmtInitTransition

import ai.newmap.util.Outcome

class ParserConfig() {
  val initState = new State(name = "INIT")
  initState.addAcceptedTransition(disconnectChannelInitTransition)
  initState.addAcceptedTransition(iterateInitTransition)
  initState.addAcceptedTransition(connectChannelInitTransition)
  initState.addAcceptedTransition(forkedVersionedStmtInitTransition)
  initState.addAcceptedTransition(dataInitTransition)
  initState.addAcceptedTransition(writeInitTransition)
  initState.addAcceptedTransition(applyCommandStmtInitTransition)
  initState.addAcceptedTransition(applyCommandsStmtInitTransition)
}

object StateMachineRunner{
  def run(tokens: Seq[Token]): Outcome[EnvStatementParse, String] = {
    val sm = new StateMachine()
    sm.run(tokens)
  }
}