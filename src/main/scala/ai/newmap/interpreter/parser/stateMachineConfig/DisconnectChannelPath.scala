package ai.newmap.interpreter.parser.stateMachineConfig

import ai.newmap.interpreter.parser.stateMachine.{State, Transition, TokenValidators}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{DisconnectChannelParse, EnvStatementParse, ParseElement}
import scala.collection.mutable.ListBuffer

object DisconnectChannelPath {
  val disconnectChannelEndState = new DisconnectChannelEndState("disconnectChannelEndState")

  val disconnectChannelIdentifierIdentifier = State("disconnectChannelIdentifierIdentifier", Vector(
    new DisconnectChannelEndStateTransition(nextState = disconnectChannelEndState)
  ))

  val disconnectChannelIdentifier = State("disconnectChannelIdentifier", Vector(
    Transition(TokenValidators.identifier, disconnectChannelIdentifierIdentifier)
  ))

  val initState = State("disconnectChannel", Vector(
    Transition(TokenValidators.identifier, disconnectChannelIdentifier)
  ))
}

class DisconnectChannelEndState(name:String) extends State(name, isEndState = true) {
  var tokenOptions: Option[List[ParseElement]] = None
  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: Option[EnvStatementParse] = {
    println("Reached DisconnectChannel Generate Parse Tree")
    val tokens = tokenOptions.get
    Some(DisconnectChannelParse(
      tokens(1).asInstanceOf[Identifier],
      tokens(2).asInstanceOf[Identifier]
    ))
  }
}

class DisconnectChannelEndStateTransition(nextState: State) extends Transition(TokenValidators.endOfInput, nextState)