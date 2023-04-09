package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.State
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Parser.StateMachineConfig.DisconnectChannelPath.disconnectChannelInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.IteratePath.iterateInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.ConnectChannelPath.connectChannelInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.ForkChannelPath.forkedVersionedStmtInitTransition

class ParserConfig {

  val initState = new State(name = "INIT")
  initState.addAcceptedTransition(disconnectChannelInitTransition)
  initState.addAcceptedTransition(iterateInitTransition)
  initState.addAcceptedTransition(connectChannelInitTransition)
  initState.addAcceptedTransition(forkedVersionedStmtInitTransition)
}

object StateMachineRunner{
  def run(tokens: Seq[Lexer.Token]) : Unit = {
    val parserConfig = new ParserConfig()
    var curState = parserConfig.initState
    tokens.foreach(token => {
      curState = curState.changeState(token)
    })
    curState = curState.changeState(null)
  }
}