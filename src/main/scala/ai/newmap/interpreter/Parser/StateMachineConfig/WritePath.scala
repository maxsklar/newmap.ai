package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, IdentifierParse, ParseElement, ParseTree, WriteToChannelParse}

import scala.collection.mutable.ListBuffer

object WritePath {

  private val initState = new State(name = "write")
  private val writeIdentifier = new State(isEndState = false, name = "writeIdentifier")
  private val writeIdentifierIdentifier = new State(isEndState = false, name = "writeIdentifierIdentifier")
  private val writeEndState = new WritePathEndState(name = "writeEndState")

  val writeInitTransition = new Transition(expectedToken = Identifier("write"), nextState = initState)
  private val writeId1Transition = new Transition(expectedTokenClass = classOf[Identifier], nextState = writeIdentifier)
  private val writeId2Transition = new Transition(nextExpectedParseTree = classOf[ParseTree], nextState = writeIdentifierIdentifier)
  private val writeEndTransition = new WritePathEndStateTransition(nextState = writeEndState)

  initState.addAcceptedTransition(writeId1Transition)
  writeIdentifier.addAcceptedTransition(writeId2Transition)
  writeIdentifierIdentifier.addAcceptedTransition(writeEndTransition)

}

class WritePathEndState(name: String) extends State(isEndState = true, name) {

  var tokenOptions: Option[List[ParseElement]] = None

  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token] = null): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    val tokens = tokenOptions.get
    WriteToChannelParse(
      tokens(1).asInstanceOf[IdentifierParse],
      tokens(2).asInstanceOf[ParseTree]
    )
  }

}

class WritePathEndStateTransition(nextState: State) extends Transition(expectedToken = null, nextState = nextState) {
  override def validateToken(t: Lexer.Token): Boolean = true
}
