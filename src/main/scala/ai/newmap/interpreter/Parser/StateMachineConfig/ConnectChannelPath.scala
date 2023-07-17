package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ConnectChannelParse, EnvStatementParse, ParseElement}
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object ConnectChannelPath {
  private val initState = State("connectChannel")
  private val connectChannelIdentifier = State("connectChannelIdentifier")
  private val connectChannelIdentifierIdentifier = State("connectChannelIdentifierIdentifier")
  private val connectChannelEndState = new ConnectChannelEndState("connectChannelEndState")

  val connectChannelInitTransition = Transition(tokenValidator = TokenValidators.specificIdentifier("connectChannel"), nextState = initState) 

  initState.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = connectChannelIdentifier))
  connectChannelIdentifier.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = connectChannelIdentifierIdentifier))
  connectChannelIdentifierIdentifier.addAcceptedTransition(new ConnectChannelEndStateTransition(nextState = connectChannelEndState))
}

class ConnectChannelEndState(name:String) extends State(name, isEndState = true){

  var tokenOptions: Option[List[ParseElement]] = None
  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: Option[EnvStatementParse] = {
    val tokens = tokenOptions.get
    Some(ConnectChannelParse(
      tokens(1).asInstanceOf[Identifier],
      tokens(2).asInstanceOf[Identifier]
    ))
  }

}

class ConnectChannelEndStateTransition(nextState: State) extends Transition(TokenValidators.endOfInput, nextState)
