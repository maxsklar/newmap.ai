package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.interpreter.Parser.StateMachine.{State, Transition}
import ai.newmap.interpreter.Parser.StateMachine.TokenValidators

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