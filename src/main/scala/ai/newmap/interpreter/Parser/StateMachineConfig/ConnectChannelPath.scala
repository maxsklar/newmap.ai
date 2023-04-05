package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ConnectChannelParse, IdentifierParse, ParseElement}

import scala.collection.mutable.ListBuffer

object ConnectChannelPath {

  private val initState = new State(name = "connectChannel")
  private val connectChannelIdentifier = new State(isEndState = false, name = "connectChannelIdentifier")
  private val connectChannelIdentifierIdentifier = new State(isEndState = false, name = "connectChannelIdentifierIdentifier")
  private val connectChannelEndState = new ConnectChannelEndState(name = "connectChannelEndState")

  val connectChannelInitTransition = new Transition(Identifier("connectChannel"), initState)
  private val connectChannelId1Transition = new Transition(Identifier("c"), connectChannelIdentifier)
  private val connectChannelId2Transition = new Transition(Identifier("c"), connectChannelIdentifierIdentifier)
  private val connectChannelEndTransition = new ConnectChannelEndStateTransition(connectChannelEndState)

  initState.addAcceptedTransition(connectChannelId1Transition)
  connectChannelIdentifier.addAcceptedTransition(connectChannelId2Transition)
  connectChannelIdentifierIdentifier.addAcceptedTransition(connectChannelEndTransition)

}

class ConnectChannelEndState(name:String) extends State(isEndState = true, name){

  override def reach(p: ListBuffer[ParseElement]): Unit = {
    val tokens = p.toList
    print(ConnectChannelParse(
      IdentifierParse(tokens(1).asInstanceOf[Identifier].id),
      IdentifierParse(tokens(2).asInstanceOf[Identifier].id)
    ))
  }

}

class ConnectChannelEndStateTransition(nextState:State) extends Transition(token = null, nextState){
  override def validateToken(t: Lexer.Token): Boolean = true
}




