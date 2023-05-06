package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ApplyCommandStatementParse, EnvStatementParse, IdentifierParse, ParseElement, ParseTree}

import scala.collection.mutable.ListBuffer
object applyCommandPath {
  private val initState = new State(isEndState = false, name = "update")
  private val applyCommandStmtIdentifier = new State(isEndState = false, name = "applyCommandStmtIdentifier")
  private val applyCommandStmtIdentifierIdentifier = new State(isEndState = false, name = "applyCommandStmtIdentifierIdentifier")
  private val applyCommandStmtEndState = new ApplyCommandStmtEndState(name = "applyCommandStmtEndState")

  val applyCommandStmtInitTransition = new Transition(expectedToken = Identifier("update"), nextState = initState)
  private val applyCommandStmtId1Transition = new Transition(expectedTokenClass = classOf[Identifier], nextState = applyCommandStmtIdentifier)
  private val applyCommandStmtId2Transition = new Transition(nextExpectedParseTree = classOf[ParseTree], nextState = applyCommandStmtIdentifierIdentifier)
  private val applyCommandStmtEndTransition = new ApplyCommandStmtEndStateTransition(nextState = applyCommandStmtEndState)

  initState.addAcceptedTransition(applyCommandStmtId1Transition)
  applyCommandStmtIdentifier.addAcceptedTransition(applyCommandStmtId2Transition)
  applyCommandStmtIdentifierIdentifier.addAcceptedTransition(applyCommandStmtEndTransition)
}

class ApplyCommandStmtEndState(name:String) extends State(isEndState = true, name){
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

class ApplyCommandStmtEndStateTransition(nextState:State) extends Transition(expectedToken = null, nextState = nextState){
  override def validateToken(t: Lexer.Token): Boolean = true
}
