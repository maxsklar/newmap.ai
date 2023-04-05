package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.State
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Parser.StateMachineConfig.DisconnectChannelPath.disconnectChannelInitTransition
import ai.newmap.interpreter.Parser.StateMachineConfig.ConnectChannelPath.connectChannelInitTransition

class ParserConfig {

  val initState = new State(name = "INIT")
  initState.addAcceptedTransition(disconnectChannelInitTransition)
  initState.addAcceptedTransition(connectChannelInitTransition)
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