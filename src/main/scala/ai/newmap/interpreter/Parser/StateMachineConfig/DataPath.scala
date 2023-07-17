package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model._
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object DataPath {

  private val initState = State("data")
  private val dataIdentifier = State("dataIdentifier")
  private val dataIdentifierIdentifier = State("dataIdentifierIdentifier")
  private val dataIdentifierIdentifierIdentifier = State("dataIdentifierIdentifierIdentifier")
  private val dataEndState = new DataEndState("dataEndState")

  val dataInitTransition = Transition(tokenValidator = TokenValidators.specificIdentifier("data"), nextState = initState)
  initState.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = dataIdentifier))
  dataIdentifier.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = dataIdentifierIdentifier))
  dataIdentifierIdentifier.addAcceptedTransition(Transition(expectingParseTree = true, nextState = dataIdentifierIdentifierIdentifier))
  dataIdentifierIdentifierIdentifier.addAcceptedTransition(new DataEndStateTransition(nextState = dataEndState))

}

class DataEndState(name: String) extends State(name,isEndState = true) {

  var tokenOptions: Option[List[ParseElement]] = None

  override def reach(p: ListBuffer[ParseElement], ts: Seq[Lexer.Token] = null): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    val tokens = tokenOptions.get
    NewTypeStatementParse(
      tokens(1).asInstanceOf[IdentifierParse],
      tokens(2).asInstanceOf[ParseTree]
    )
  }


}

class DataEndStateTransition(nextState: State) extends Transition(nextState = nextState)
