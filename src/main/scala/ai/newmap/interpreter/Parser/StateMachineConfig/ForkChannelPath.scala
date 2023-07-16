package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, ForkedVersionedStatementParse, IdentifierParse, ParseElement}
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object ForkChannelPath {
  private val initState = new State(name = "fork")
  private val forkedVersionedStmtIdentifier = new State(isEndState = false, name = "forkedVersionedStmtIdentifier")
  private val forkedVersionedStmtIdentifierIdentifier = new State(isEndState = false, name = "forkedVersionedStmtIdentifierIdentifier")
  private val forkedVersionedStmtIdentifierIdentifierIdentifier = new State(isEndState = false, name = "forkedVersionedStmtIdentifierIdentifierIdentifier")
  private val forkedVersionedStmtEndState = new ForkedVersionedStmtEndState(name = "forkedVersionedStmtEndState")


  val forkedVersionedStmtInitTransition = new Transition(tokenValidator = TokenValidators.specificIdentifier("fork"), nextState = null)
  private val forkedVersionedStmtId1Transition = new Transition(tokenValidator = TokenValidators.identifier, nextState = forkedVersionedStmtIdentifier)
  private val forkedVersionedStmtId2Transition = new Transition(tokenValidator = TokenValidators.specificIdentifier("as"), nextState = forkedVersionedStmtIdentifierIdentifier)
  private val forkedVersionedStmtId3Transition = new Transition(tokenValidator = TokenValidators.identifier, nextState = forkedVersionedStmtIdentifierIdentifierIdentifier)
  private val forkedVersionedStmtEndTransition = new ForkedVersionedStmtEndStateTransition(nextState = forkedVersionedStmtEndState)

  initState.addAcceptedTransition(forkedVersionedStmtId1Transition)
  forkedVersionedStmtIdentifier.addAcceptedTransition(forkedVersionedStmtId2Transition)
  forkedVersionedStmtIdentifierIdentifier.addAcceptedTransition(forkedVersionedStmtId3Transition)
  forkedVersionedStmtIdentifierIdentifierIdentifier.addAcceptedTransition(forkedVersionedStmtEndTransition)

}

class ForkedVersionedStmtEndState(name:String) extends State(isEndState = true, name){

  var tokenOptions: Option[List[ParseElement]] = None
  override def reach(p: ListBuffer[ParseElement], ts:Seq[Lexer.Token] = null): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    val tokens = tokenOptions.get
    ForkedVersionedStatementParse(

      tokens(1).asInstanceOf[Identifier],
      tokens(2).asInstanceOf[Identifier]
    )
  }

}

class ForkedVersionedStmtEndStateTransition(nextState:State) extends Transition(nextState = nextState){
  override def validateToken(t: Lexer.Token): Boolean = true
}
