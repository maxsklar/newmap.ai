package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.interpreter.Parser.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ApplyCommandStatementParse, EnvStatementParse, IdentifierParse, ParseElement, ParseTree}
import ai.newmap.interpreter.Parser.StateMachine.TokenValidators

import scala.collection.mutable.ListBuffer
object ApplyCommandPath {
  val applyCommandStmtEndState = new ApplyCommandStmtEndState("applyCommandStmtEndState")

  val applyCommandStmtIdentifierIdentifier = State("applyCommandStmtIdentifierIdentifier", Vector(
    new ApplyCommandStmtEndStateTransition(nextState = applyCommandStmtEndState)
  ))

  val applyCommandStmtIdentifier = State("applyCommandStmtIdentifier", Vector(
    Transition(expectingParseTree = true, nextState = applyCommandStmtIdentifierIdentifier)
  ))

  val initState = State("update", Vector(
    Transition(TokenValidators.identifier, applyCommandStmtIdentifier)
  ))
}

class ApplyCommandStmtEndState(name:String) extends State(name){
  var tokenOptions: Option[List[ParseElement]] = None

  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: Option[EnvStatementParse] = {
    println("Reached ApplyCommand Generate Parse Tree")
    val tokens = tokenOptions.get
    Some(ApplyCommandStatementParse(
      tokens(1).asInstanceOf[IdentifierParse],
      tokens(2).asInstanceOf[ParseTree]
    ))
  }
}

class ApplyCommandStmtEndStateTransition(nextState: State) extends Transition(TokenValidators.endOfInput, nextState)
