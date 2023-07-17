package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, StateMachine, Transition}
import ai.newmap.interpreter.Lexer.Token
import ai.newmap.model.EnvStatementParse
import ai.newmap.StateMachine.TokenValidators
import ai.newmap.util.Outcome

class ParserConfig() {
  val initState = State("INIT", Vector(
    Transition(TokenValidators.specificIdentifier("disconnectChannel"), DisconnectChannelPath.initState),
    Transition(TokenValidators.specificIdentifier("iterate"), IteratePath.initState),
    Transition(TokenValidators.specificIdentifier("connectChannel"), ConnectChannelPath.initState) ,
    Transition(TokenValidators.specificIdentifier("fork"), ForkChannelPath.initState),
    Transition(TokenValidators.specificIdentifier("data"), DataPath.initState),
    Transition(TokenValidators.specificIdentifier("write"), WritePath.initState),
    Transition(TokenValidators.specificIdentifier("update"), ApplyCommandPath.initState),
    Transition(TokenValidators.specificIdentifier("updates"), ApplyCommandsPath.initState)
  ))
}

object StateMachineRunner{
  def run(tokens: Seq[Token]): Outcome[EnvStatementParse, String] = {
    val sm = new StateMachine()
    sm.run(tokens)
  }
}