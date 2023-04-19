package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ConnectChannelParse, EnvStatementParse, ParseElement}

import scala.collection.mutable.ListBuffer

object ConnectChannelPath {

  private val initState = new State(name = "connectChannel")
  private val connectChannelIdentifier = new State(isEndState = false, name = "connectChannelIdentifier")
  private val connectChannelIdentifierIdentifier = new State(isEndState = false, name = "connectChannelIdentifierIdentifier")
  private val connectChannelEndState = new ConnectChannelEndState(name = "connectChannelEndState")

  val connectChannelInitTransition = new Transition(expectedToken = Identifier("connectChannel"), nextState = initState)
  private val connectChannelId1Transition = new Transition(expectedClass = classOf[Identifier], nextState = connectChannelIdentifier)
  private val connectChannelId2Transition = new Transition(expectedClass = classOf[Identifier], nextState = connectChannelIdentifierIdentifier)
  private val connectChannelEndTransition = new ConnectChannelEndStateTransition(nextState = connectChannelEndState)

  initState.addAcceptedTransition(connectChannelId1Transition)
  connectChannelIdentifier.addAcceptedTransition(connectChannelId2Transition)
  connectChannelIdentifierIdentifier.addAcceptedTransition(connectChannelEndTransition)

}

class ConnectChannelEndState(name:String) extends State(isEndState = true, name){

  var tokenOptions: Option[List[ParseElement]] = None
  override def reach(p: ListBuffer[ParseElement]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    val tokens = tokenOptions.get
    ConnectChannelParse(
      tokens(1).asInstanceOf[Identifier],
      tokens(2).asInstanceOf[Identifier]
    )
  }

}

class ConnectChannelEndStateTransition(nextState:State) extends Transition(expectedToken = null, nextState = nextState){
  override def validateToken(t: Lexer.Token): Boolean = true
}




