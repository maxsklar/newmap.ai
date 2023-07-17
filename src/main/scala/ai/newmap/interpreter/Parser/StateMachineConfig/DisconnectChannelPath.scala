package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{DisconnectChannelParse, EnvStatementParse, ParseElement}
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object DisconnectChannelPath {
  private val initState = State("disconnectChannel")
  private val disconnectChannelIdentifier = State("disconnectChannelIdentifier")
  private val disconnectChannelIdentifierIdentifier = State("disconnectChannelIdentifierIdentifier")
  private val disconnectChannelEndState = new DisconnectChannelEndState("disconnectChannelEndState")

  val disconnectChannelInitTransition = Transition(tokenValidator = TokenValidators.specificIdentifier("disconnectChannel"), nextState = initState)

  initState.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = disconnectChannelIdentifier))
  disconnectChannelIdentifier.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = disconnectChannelIdentifierIdentifier))
  disconnectChannelIdentifierIdentifier.addAcceptedTransition(new DisconnectChannelEndStateTransition(nextState = disconnectChannelEndState))
}

class DisconnectChannelEndState(name:String) extends State(name, isEndState = true) {
  var tokenOptions: Option[List[ParseElement]] = None
  override def reach(p: ListBuffer[ParseElement], ts:Seq[Lexer.Token] = null): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    println("Reached DisconnectChannel Generate Parse Tree")
    val tokens = tokenOptions.get
    DisconnectChannelParse(
      tokens(1).asInstanceOf[Identifier],
      tokens(2).asInstanceOf[Identifier]
    )
  }
}

class DisconnectChannelEndStateTransition(nextState: State) extends Transition(nextState = nextState)