package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ApplyCommandStatementParse, EnvStatementParse, IdentifierParse, ParseElement, ParseTree}
import ai.newmap.StateMachine.TokenValidators

import scala.collection.mutable.ListBuffer
object applyCommandPath {
  private val initState = State("update")
  private val applyCommandStmtIdentifier = State("applyCommandStmtIdentifier")
  private val applyCommandStmtIdentifierIdentifier = State("applyCommandStmtIdentifierIdentifier")
  private val applyCommandStmtEndState = new ApplyCommandStmtEndState("applyCommandStmtEndState")

  val applyCommandStmtInitTransition = Transition(tokenValidator = TokenValidators.specificIdentifier("update"), nextState = initState)

  initState.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = applyCommandStmtIdentifier))
  applyCommandStmtIdentifier.addAcceptedTransition(Transition(expectingParseTree = true, nextState = applyCommandStmtIdentifierIdentifier))
  applyCommandStmtIdentifierIdentifier.addAcceptedTransition(new ApplyCommandStmtEndStateTransition(nextState = applyCommandStmtEndState))
}

class ApplyCommandStmtEndState(name:String) extends State(name, isEndState = true){
  var tokenOptions: Option[List[ParseElement]] = None

  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token] = null): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    println("Reached ApplyCommand Generate Parse Tree")
    val tokens = tokenOptions.get
    ApplyCommandStatementParse(
      tokens(1).asInstanceOf[IdentifierParse],
      tokens(2).asInstanceOf[ParseTree]
    )
  }

}

class ApplyCommandStmtEndStateTransition(nextState: State) extends Transition(nextState = nextState)
