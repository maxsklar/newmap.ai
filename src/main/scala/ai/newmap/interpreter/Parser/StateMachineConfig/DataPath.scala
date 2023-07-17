package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model._
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object DataPath {
  val dataEndState = new DataEndState("dataEndState")

  val dataIdentifierIdentifierIdentifier = State("dataIdentifierIdentifierIdentifier", Vector(
    new DataEndStateTransition(nextState = dataEndState)
  ))

  val dataIdentifierIdentifier = State("dataIdentifierIdentifier", Vector(
    Transition(expectingParseTree = true, nextState = dataIdentifierIdentifierIdentifier)
  ))

  val dataIdentifier = State("dataIdentifier", Vector(
    Transition(TokenValidators.identifier, dataIdentifierIdentifier)
  ))

  val initState = State("data", Vector(
    Transition(TokenValidators.identifier, dataIdentifier)
  ))
}

class DataEndState(name: String) extends State(name,isEndState = true) {

  var tokenOptions: Option[List[ParseElement]] = None

  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: Option[EnvStatementParse] = {
    val tokens = tokenOptions.get
    Some(NewTypeStatementParse(
      tokens(1).asInstanceOf[IdentifierParse],
      tokens(2).asInstanceOf[ParseTree]
    ))
  }


}

class DataEndStateTransition(nextState: State) extends Transition(TokenValidators.endOfInput, nextState)
