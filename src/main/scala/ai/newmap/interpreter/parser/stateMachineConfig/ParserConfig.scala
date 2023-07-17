package ai.newmap.interpreter.parser.stateMachineConfig

import ai.newmap.interpreter.parser.stateMachine.{State, Transition, TokenValidators}

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