package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ApplyCommandsStatementParse, EnvStatementParse, IdentifierParse, ParseElement, ParseTree}
import ai.newmap.StateMachine.TokenValidators

import scala.collection.mutable.ListBuffer
object ApplyCommandsPath {
  val applyCommandsStmtEndState = new ApplyCommandsStmtEndState("applyCommandsStmtEndState")

  val applyCommandsStmtIdentifierIdentifier = State("applyCommandsStmtIdentifierIdentifier", Vector(
    new ApplyCommandsStmtEndStateTransition(nextState = applyCommandsStmtEndState)
  ))
  
  val applyCommandsStmtIdentifier = State("applyCommandsStmtIdentifier", Vector(
    Transition(expectingParseTree = true, nextState = applyCommandsStmtIdentifierIdentifier)
  ))

  val initState = State("updates", Vector(
    Transition(TokenValidators.identifier, applyCommandsStmtIdentifier)
  ))
}

class ApplyCommandsStmtEndState(name:String) extends State(name, isEndState = true){
  var tokenOptions: Option[List[ParseElement]] = None

  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: Option[EnvStatementParse] = {
    println("Reached ApplyCommand Generate Parse Tree")
    val tokens = tokenOptions.get
    Some(ApplyCommandsStatementParse(
      tokens(1).asInstanceOf[IdentifierParse],
      tokens(2).asInstanceOf[ParseTree]
    ))
  }

}

class ApplyCommandsStmtEndStateTransition(nextState: State) extends Transition(TokenValidators.endOfInput, nextState)
