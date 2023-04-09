package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ForkedVersionedStatementParse, IdentifierParse, ParseElement}

import scala.collection.mutable.ListBuffer

object ForkChannelPath {
  private val initState = new State(name = "fork")
  private val forkedVersionedStmtIdentifier = new State(isEndState = false, name = "forkedVersionedStmtIdentifier")
  private val forkedVersionedStmtIdentifierIdentifier = new State(isEndState = false, name = "forkedVersionedStmtIdentifierIdentifier")
  private val forkedVersionedStmtIdentifierIdentifierIdentifier = new State(isEndState = false, name = "forkedVersionedStmtIdentifierIdentifierIdentifier")
  private val forkedVersionedStmtEndState = new ForkedVersionedStmtEndState(name = "forkedVersionedStmtEndState")

  val forkedVersionedStmtInitTransition = new Transition(Identifier("fork"), initState)
  private val forkedVersionedStmtId1Transition = new Transition(Identifier("c"), forkedVersionedStmtIdentifier)
  private val forkedVersionedStmtId2Transition = new Transition(Identifier("as"), forkedVersionedStmtIdentifierIdentifier)
  private val forkedVersionedStmtId3Transition = new Transition(Identifier("c"), forkedVersionedStmtIdentifierIdentifierIdentifier)
  private val forkedVersionedStmtEndTransition = new DisconnectChannelEndStateTransition(forkedVersionedStmtEndState)

  initState.addAcceptedTransition(forkedVersionedStmtId1Transition)
  forkedVersionedStmtIdentifier.addAcceptedTransition(forkedVersionedStmtId2Transition)
  forkedVersionedStmtIdentifierIdentifier.addAcceptedTransition(forkedVersionedStmtId3Transition)
  forkedVersionedStmtIdentifierIdentifierIdentifier.addAcceptedTransition(forkedVersionedStmtEndTransition)

  class ForkedVersionedStmtEndState(name:String) extends State(isEndState = true, name){

    override def reach(p: ListBuffer[ParseElement]): Unit = {
      val tokens = p.toList
      print(ForkedVersionedStatementParse(
        IdentifierParse(tokens(1).asInstanceOf[Identifier].id),
        IdentifierParse(tokens(2).asInstanceOf[Identifier].id)
      ))
    }

  }

  class ForkedVersionedStmtEndStateTransition(nextState:State) extends Transition(token = null, nextState){
    override def validateToken(t: Lexer.Token): Boolean = true
  }
}
