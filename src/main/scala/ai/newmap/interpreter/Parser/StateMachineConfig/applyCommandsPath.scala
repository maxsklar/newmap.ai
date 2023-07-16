package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ApplyCommandsStatementParse, EnvStatementParse, IdentifierParse, ParseElement, ParseTree}
import ai.newmap.StateMachine.TokenValidators

import scala.collection.mutable.ListBuffer
object applyCommandsPath {
  private val initState = new State(isEndState = false, name = "updates")
  private val applyCommandsStmtIdentifier = new State(isEndState = false, name = "applyCommandsStmtIdentifier")
  private val applyCommandsStmtIdentifierIdentifier = new State(isEndState = false, name = "applyCommandsStmtIdentifierIdentifier")
  private val applyCommandsStmtEndState = new ApplyCommandsStmtEndState(name = "applyCommandsStmtEndState")

  val applyCommandsStmtInitTransition = new Transition(tokenValidator = TokenValidators.specificIdentifier("updates"), nextState = initState)
  private val applyCommandsStmtId1Transition = new Transition(tokenValidator = TokenValidators.identifier, nextState = applyCommandsStmtIdentifier)
  private val applyCommandsStmtId2Transition = new Transition(nextExpectedParseTree = classOf[ParseTree], nextState = applyCommandsStmtIdentifierIdentifier)
  private val applyCommandsStmtEndTransition = new ApplyCommandsStmtEndStateTransition(nextState = applyCommandsStmtEndState)

  initState.addAcceptedTransition(applyCommandsStmtId1Transition)
  applyCommandsStmtIdentifier.addAcceptedTransition(applyCommandsStmtId2Transition)
  applyCommandsStmtIdentifierIdentifier.addAcceptedTransition(applyCommandsStmtEndTransition)
}

class ApplyCommandsStmtEndState(name:String) extends State(isEndState = true, name){
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

class ApplyCommandsStmtEndStateTransition(nextState:State) extends Transition(nextState = nextState){
  override def validateToken(t: Lexer.Token): Boolean = true
}
