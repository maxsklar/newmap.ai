package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{DisconnectChannelParse, EnvStatementParse, ParseElement}
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object DisconnectChannelPath {
  private val initState = new State(isEndState = false, name = "disconnectChannel")
  private val disconnectChannelIdentifier = new State(isEndState = false, name = "disconnectChannelIdentifier")
  private val disconnectChannelIdentifierIdentifier = new State(isEndState = false, name = "disconnectChannelIdentifierIdentifier")
  private val disconnectChannelEndState = new DisconnectChannelEndState(name = "disconnectChannelEndState")

  val disconnectChannelInitTransition = new Transition(tokenValidator = TokenValidators.specificIdentifier("disconnectChannel"), nextState = initState)
  private val disconnectChannelId1Transition = new Transition(tokenValidator = TokenValidators.identifier, nextState = disconnectChannelIdentifier)
  private val disconnectChannelId2Transition = new Transition(tokenValidator = TokenValidators.identifier, nextState = disconnectChannelIdentifierIdentifier)
  private val disconnectChannelEndTransition = new DisconnectChannelEndStateTransition(nextState = disconnectChannelEndState)

  initState.addAcceptedTransition(disconnectChannelId1Transition)
  disconnectChannelIdentifier.addAcceptedTransition(disconnectChannelId2Transition)
  disconnectChannelIdentifierIdentifier.addAcceptedTransition(disconnectChannelEndTransition)
}

class DisconnectChannelEndState(name:String) extends State(isEndState = true, name){
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

class DisconnectChannelEndStateTransition(nextState:State) extends Transition(nextState = nextState){
  override def validateToken(t: Lexer.Token): Boolean = true
}
