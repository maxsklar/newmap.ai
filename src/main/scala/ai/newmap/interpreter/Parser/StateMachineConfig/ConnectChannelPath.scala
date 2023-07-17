package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.interpreter.Parser.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ConnectChannelParse, EnvStatementParse, ParseElement}
import ai.newmap.interpreter.Parser.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object ConnectChannelPath {
  val connectChannelEndState = new ConnectChannelEndState("connectChannelEndState")

  val connectChannelIdentifierIdentifier = State("connectChannelIdentifierIdentifier", Vector(
    new ConnectChannelEndStateTransition(nextState = connectChannelEndState)
  ))

  val connectChannelIdentifier = State("connectChannelIdentifier", Vector(
    Transition(TokenValidators.identifier, connectChannelIdentifierIdentifier)
  ))

  val initState = State("connectChannel", Vector(
    Transition(TokenValidators.identifier, connectChannelIdentifier)
  ))
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
