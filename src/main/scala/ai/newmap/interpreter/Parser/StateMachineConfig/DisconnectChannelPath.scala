package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{DisconnectChannelParse, IdentifierParse, ParseElement}

import scala.collection.mutable.ListBuffer

object DisconnectChannelPath {

  private val initState = new State(name = "disconnectChannel")
  private val disconnectChannelIdentifier = new State(isEndState = false, name = "disconnectChannelIdentifier")
  private val disconnectChannelIdentifierIdentifier = new State(isEndState = false, name = "disconnectChannelIdentifierIdentifier")
  private val disconnectChannelEndState = new DisconnectChannelEndState(name = "disconnectChannelEndState")

  val disconnectChannelInitTransition = new Transition(Identifier("disconnectChannel"), initState)
  private val disconnectChannelId1Transition = new Transition(Identifier("c"), disconnectChannelIdentifier)
  private val disconnectChannelId2Transition = new Transition(Identifier("c"), disconnectChannelIdentifierIdentifier)
  private val disconnectChannelEndTransition = new DisconnectChannelEndStateTransition(disconnectChannelEndState)

  initState.addAcceptedTransition(disconnectChannelId1Transition)
  disconnectChannelIdentifier.addAcceptedTransition(disconnectChannelId2Transition)
  disconnectChannelIdentifierIdentifier.addAcceptedTransition(disconnectChannelEndTransition)

//  def acceptAnyIdentifier():Lexer.Token = {
//
//  }
//
}

class DisconnectChannelEndState(name:String) extends State(isEndState = true, name){

  override def reach(p: ListBuffer[ParseElement]): Unit = {
    val tokens = p.toList
    print(DisconnectChannelParse(
      IdentifierParse(tokens(1).asInstanceOf[Identifier].id),
      IdentifierParse(tokens(2).asInstanceOf[Identifier].id)
    ))
  }

}

class DisconnectChannelEndStateTransition(nextState:State) extends Transition(token = null, nextState){
  override def validateToken(t: Lexer.Token): Boolean = true
}
