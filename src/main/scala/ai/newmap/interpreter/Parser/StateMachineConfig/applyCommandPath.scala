package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ApplyCommandStatementParse, IdentifierParse, ParseElement}

import scala.collection.mutable.ListBuffer

object applyCommandPath {
//  private val initState = new State(name = "update")
//  private val applyCommandStmtIdentifier = new State(isEndState = false, name = "applyCommandStmtIdentifier")
//  private val applyCommandStmtIdentifierIdentifier = new State(isEndState = false, name = "applyCommandStmtIdentifierIdentifier")
//  //Parse Tree Result --> Nested State Machine Initialization
//  // O/P is a parseTree of List of Operations
//
//  private val applyCommandStmtIdentifierIdentifierIdentifier = new State(isEndState = false, name = "applyCommandStmtIdentifierIdentifierIdentifier")
//  private val applyCommandStmtEndState = new ApplyCommandStmtEndState(name = "applyCommandStmtEndState")
//
//  val applyCommandStmtInitTransition = new Transition(Identifier("update"), initState)
//  private val applyCommandStmtId1Transition = new Transition(Identifier("c"), applyCommandStmtIdentifier)
//  private val applyCommandStmtId2Transition = new Transition(Expectation, applyCommandStmtIdentifierIdentifier)
//  private val applyCommandStmtId3Transition = new Transition(Identifier("c"), applyCommandStmtIdentifierIdentifierIdentifier)
//  private val applyCommandStmtEndTransition = new DisconnectChannelEndStateTransition(applyCommandStmtEndState)
//
//  initState.addAcceptedTransition(applyCommandStmtId1Transition)
//  applyCommandStmtIdentifier.addAcceptedTransition(applyCommandStmtId2Transition)
//  applyCommandStmtIdentifierIdentifier.addAcceptedTransition(applyCommandStmtId3Transition)
//  applyCommandStmtIdentifierIdentifierIdentifier.addAcceptedTransition(applyCommandStmtEndTransition)
}

class ApplyCommandStmtEndState(name:String) extends State(isEndState = true, name){

//  override def reach(p: ListBuffer[ParseElement]): Unit = {
//    val tokens = p.toList
//    print(ApplyCommandStatementParse(
//      IdentifierParse(tokens(1).asInstanceOf[Identifier].id),
//      IdentifierParse(tokens(2).asInstanceOf[Identifier].id)
//    ))
//  }

}

//class ApplyCommandStmtEndStateTransition(nextState:State) extends Transition(token = null, nextState){
//  override def validateToken(t: Lexer.Token): Boolean = true
//}
