package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, ForkedVersionedStatementParse, IdentifierParse, ParseElement}
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object ForkChannelPath {
  private val initState = State("fork")
  private val forkedVersionedStmtIdentifier = State("forkedVersionedStmtIdentifier")
  private val forkedVersionedStmtIdentifierIdentifier = State("forkedVersionedStmtIdentifierIdentifier")
  private val forkedVersionedStmtIdentifierIdentifierIdentifier = State("forkedVersionedStmtIdentifierIdentifierIdentifier")
  private val forkedVersionedStmtEndState = new ForkedVersionedStmtEndState("forkedVersionedStmtEndState")

  val forkedVersionedStmtInitTransition = Transition(tokenValidator = TokenValidators.specificIdentifier("fork"), nextState = initState)

  initState.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = forkedVersionedStmtIdentifier))
  forkedVersionedStmtIdentifier.addAcceptedTransition(Transition(tokenValidator = TokenValidators.specificIdentifier("as"), nextState = forkedVersionedStmtIdentifierIdentifier))
  forkedVersionedStmtIdentifierIdentifier.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = forkedVersionedStmtIdentifierIdentifierIdentifier))
  forkedVersionedStmtIdentifierIdentifierIdentifier.addAcceptedTransition(new ForkedVersionedStmtEndStateTransition(nextState = forkedVersionedStmtEndState))
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
