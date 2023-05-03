package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model._

import scala.collection.mutable.ListBuffer

object DataPath {

  private val initState = new State(name = "data")
  private val dataIdentifier = new State(isEndState = false, name = "dataIdentifier")
  private val dataIdentifierIdentifier = new State(isEndState = false, name = "dataIdentifierIdentifier")
  private val dataIdentifierIdentifierIdentifier = new State(isEndState = false, name = "dataIdentifierIdentifierIdentifier")
  private val dataEndState = new DataEndState(name = "dataEndState")

  val dataInitTransition = new Transition(expectedToken = Identifier("data"), nextState = initState)
  private val dataId1Transition = new Transition(expectedTokenClass = classOf[Identifier], nextState = dataIdentifier)
  private val dataId2Transition = new Transition(expectedTokenClass = classOf[Identifier], nextState = dataIdentifierIdentifier)
  private val dataId3Transition = new Transition(nextExpectedParseTree = classOf[ParseTree], nextState = dataIdentifierIdentifierIdentifier)
  private val dataEndTransition = new DataEndStateTransition(nextState = dataEndState)

  initState.addAcceptedTransition(dataId1Transition)
  dataIdentifier.addAcceptedTransition(dataId2Transition)
  dataIdentifierIdentifier.addAcceptedTransition(dataId3Transition)
  dataIdentifierIdentifierIdentifier.addAcceptedTransition(dataEndTransition)

}

class DataEndState(name: String) extends State(isEndState = true, name) {

  var tokenOptions: Option[List[ParseElement]] = None

  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token] = null): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    val tokens = tokenOptions.get
    NewTypeStatementParse(
      tokens(1).asInstanceOf[IdentifierParse],
      tokens(2).asInstanceOf[ParseTree]
    )
  }


}

class DataEndStateTransition(nextState: State) extends Transition(expectedToken = null, nextState = nextState) {
  override def validateToken(t: Lexer.Token): Boolean = true
}
