package ai.newmap.interpreter.parser.stateMachineConfig

import ai.newmap.interpreter.parser.stateMachine.{State, Transition, TokenValidators}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, ForkedVersionedStatementParse, IdentifierParse, ParseElement}
import scala.collection.mutable.ListBuffer

object ForkChannelPath {
  val forkedVersionedStmtEndState = new ForkedVersionedStmtEndState("forkedVersionedStmtEndState")

  val forkedVersionedStmtIdentifierIdentifierIdentifier = State("forkedVersionedStmtIdentifierIdentifierIdentifier", Vector(
    new ForkedVersionedStmtEndStateTransition(nextState = forkedVersionedStmtEndState)
  ))

  val forkedVersionedStmtIdentifierIdentifier = State("forkedVersionedStmtIdentifierIdentifier", Vector(
    Transition(TokenValidators.identifier, forkedVersionedStmtIdentifierIdentifierIdentifier)
  ))

  val forkedVersionedStmtIdentifier = State("forkedVersionedStmtIdentifier", Vector(
    Transition(TokenValidators.specificIdentifier("as"), forkedVersionedStmtIdentifierIdentifier)
  ))

  val initState = State("fork", Vector(
    Transition(TokenValidators.identifier, forkedVersionedStmtIdentifier)
  ))
}

class ForkedVersionedStmtEndState(name: String) extends State(name, isEndState = true){
  var tokenOptions: Option[List[ParseElement]] = None
  override def reach(p: ListBuffer[ParseElement], ts:Seq[Lexer.Token]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: Option[EnvStatementParse] = {
    val tokens = tokenOptions.get
    Some(ForkedVersionedStatementParse(
      tokens(3).asInstanceOf[Identifier],
      tokens(1).asInstanceOf[Identifier]
    ))
  }

}

class ForkedVersionedStmtEndStateTransition(nextState:State) extends Transition(TokenValidators.endOfInput, nextState)
