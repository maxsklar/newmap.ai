package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, IdentifierParse, ParseElement, ParseTree, WriteToChannelParse}
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object WritePath {

  private val initState = State("write")
  private val writeIdentifier = State("writeIdentifier")
  private val writeIdentifierIdentifier = State("writeIdentifierIdentifier")
  private val writeEndState = new WritePathEndState("writeEndState")

  val writeInitTransition = Transition(tokenValidator = TokenValidators.specificIdentifier("write"), nextState = initState)

  initState.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = writeIdentifier))
  writeIdentifier.addAcceptedTransition(Transition(expectingParseTree = true, nextState = writeIdentifierIdentifier))
  writeIdentifierIdentifier.addAcceptedTransition(new WritePathEndStateTransition(nextState = writeEndState))
}

class WritePathEndState(name: String) extends State(name, isEndState = true) {
  var tokenOptions: Option[List[ParseElement]] = None

  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: Option[EnvStatementParse] = {
    val tokens = tokenOptions.get
    Some(WriteToChannelParse(
      tokens(1).asInstanceOf[IdentifierParse],
      tokens(2).asInstanceOf[ParseTree]
    ))
  }
}

class WritePathEndStateTransition(nextState: State) extends Transition(TokenValidators.endOfInput, nextState)