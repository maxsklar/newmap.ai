package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, IdentifierParse, IterateIntoStatementParse, ParseElement}

import scala.collection.mutable.ListBuffer

object IteratePath {

  private val initState = new State(name = "iterate")
  private val iterateIdentifier = new State(isEndState = false, name = "iterateIdentifier")
  private val iterateIdentifierIdentifier = new State(isEndState = false, name = "iterateIdentifierIdentifier")
  private val iterateIdentifierIdentifierIdentifier = new State(isEndState = false, name = "iterateIdentifierIdentifierIdentifier")
  private val iterateEndState = new IterateEndState(name = "iterateEndState")

  val iterateInitTransition = new Transition(expectedToken = Identifier("iterate"), nextState = initState)
  private val iterateId1Transition = new Transition(expectedClass = classOf[Identifier], nextState = iterateIdentifier)
  private val iterateId2Transition = new Transition(expectedToken = Identifier("into"), nextState = iterateIdentifierIdentifier)
  private val iterateId3Transition = new Transition(expectedClass = classOf[Identifier], nextState = iterateIdentifierIdentifierIdentifier)
  private val iterateEndTransition = new IterateEndStateTransition(nextState = iterateEndState)

  initState.addAcceptedTransition(iterateId1Transition)
  iterateIdentifier.addAcceptedTransition(iterateId2Transition)
  iterateIdentifierIdentifier.addAcceptedTransition(iterateId3Transition)
  iterateIdentifierIdentifierIdentifier.addAcceptedTransition(iterateEndTransition)

}

class IterateEndState(name:String) extends State(isEndState = true, name){

  var tokenOptions: Option[List[ParseElement]] = None
  override def reach(p: ListBuffer[ParseElement]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    val tokens = tokenOptions.get
    IterateIntoStatementParse(
      tokens(1).asInstanceOf[Identifier],
      tokens(2).asInstanceOf[Identifier]
    )
  }


}

class IterateEndStateTransition(nextState:State) extends Transition(expectedToken = null, nextState = nextState){
  override def validateToken(t: Lexer.Token): Boolean = true
}
