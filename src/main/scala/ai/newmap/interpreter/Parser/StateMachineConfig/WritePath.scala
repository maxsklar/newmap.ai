package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, IdentifierParse, ParseElement, ParseTree, WriteToChannelParse}
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object WritePath {
  val writeEndState = new WritePathEndState("writeEndState")
  val writeIdentifierIdentifier = State("writeIdentifierIdentifier", Vector(
    new WritePathEndStateTransition(nextState = writeEndState)
  ))

  val writeIdentifier = State("writeIdentifier", Vector(
    Transition(expectingParseTree = true, nextState = writeIdentifierIdentifier)
  ))

  val initState = State("write", Vector(Transition(TokenValidators.identifier, writeIdentifier)))
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