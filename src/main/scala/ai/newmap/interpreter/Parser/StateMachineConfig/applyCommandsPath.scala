package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ApplyCommandsStatementParse, EnvStatementParse, IdentifierParse, ParseElement, ParseTree}
import ai.newmap.StateMachine.TokenValidators

import scala.collection.mutable.ListBuffer
object applyCommandsPath {
  private val initState = State("updates")
  private val applyCommandsStmtIdentifier = State("applyCommandsStmtIdentifier")
  private val applyCommandsStmtIdentifierIdentifier = State("applyCommandsStmtIdentifierIdentifier")
  private val applyCommandsStmtEndState = new ApplyCommandsStmtEndState("applyCommandsStmtEndState")

  val applyCommandsStmtInitTransition = Transition(tokenValidator = TokenValidators.specificIdentifier("updates"), nextState = initState)

  initState.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = applyCommandsStmtIdentifier))
  applyCommandsStmtIdentifier.addAcceptedTransition(Transition(expectingParseTree = true, nextState = applyCommandsStmtIdentifierIdentifier))
  applyCommandsStmtIdentifierIdentifier.addAcceptedTransition(new ApplyCommandsStmtEndStateTransition(nextState = applyCommandsStmtEndState))
}

class ApplyCommandsStmtEndState(name:String) extends State(name, isEndState = true){
  var tokenOptions: Option[List[ParseElement]] = None

  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token] = null): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    println("Reached ApplyCommand Generate Parse Tree")
    val tokens = tokenOptions.get
    ApplyCommandsStatementParse(
      tokens(1).asInstanceOf[IdentifierParse],
      tokens(2).asInstanceOf[ParseTree]
    )
  }

}

class ApplyCommandsStmtEndStateTransition(nextState: State) extends Transition(nextState = nextState)
