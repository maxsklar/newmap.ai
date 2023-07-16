package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Identifier, Number}
import ai.newmap.model.{EnvStatementParse, IterateIntoStatementParse, ParseElement, ParseTree, IdentifierParse, NaturalNumberParse}

import scala.collection.mutable.ListBuffer

object IteratePath {

  val initState = new State(name = "iterate")
  val iterateIdentifier = new State(isEndState = false, name = "iterateIdentifier")
  val iterateIndexSize = new State(isEndState = false, name = "iterateIndexSize")

  val iterateIdentifierIdentifier = new State(isEndState = false, name = "iterateIdentifierIdentifier")
  val iterateIdentifierIdentifierIdentifier = new State(isEndState = false, name = "iterateIdentifierIdentifierIdentifier")
  val iterateEndState = new IterateEndState(name = "iterateEndState")

  val iterateInitTransition = new Transition(expectedToken = Identifier("iterate"), nextState = initState)
  val iterateId1Transition = new Transition(expectedTokenClass = classOf[Identifier], nextState = iterateIdentifier)
  val iterateId1TransitionNumber = new Transition(expectedTokenClass = classOf[Number], nextState = iterateIndexSize)

  val iterateId2Transition = new Transition(expectedToken = Identifier("into"), nextState = iterateIdentifierIdentifier)
  val iterateId3Transition = new Transition(expectedTokenClass = classOf[Identifier], nextState = iterateIdentifierIdentifierIdentifier)
  val iterateEndTransition = new IterateEndStateTransition(nextState = iterateEndState)

  initState.addAcceptedTransition(iterateId1Transition)
  initState.addAcceptedTransition(iterateId1TransitionNumber)

  iterateIdentifier.addAcceptedTransition(iterateId2Transition)
  iterateIndexSize.addAcceptedTransition(iterateId2Transition)
  iterateIdentifierIdentifier.addAcceptedTransition(iterateId3Transition)
  iterateIdentifierIdentifierIdentifier.addAcceptedTransition(iterateEndTransition)
}

class IterateEndState(name:String) extends State(isEndState = true, name){

  var tokenOptions: Option[List[ParseElement]] = None
  override def reach(p: ListBuffer[ParseElement], ts:Seq[Lexer.Token] = null): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: EnvStatementParse = {
    val tokens = tokenOptions.get

    val iterableObject = tokens(1) match {
      case (a: Identifier) => IdentifierParse(a.s)
      case (a: Number) => NaturalNumberParse(a.i)
      case x => throw new Exception(s"Unexpected token for iterate parse token: $x")
    }

    IterateIntoStatementParse(
      iterableObject,
      tokens(3).asInstanceOf[Identifier]
    )
  }
}

class IterateEndStateTransition(nextState:State) extends Transition(expectedToken = null, nextState = nextState){
  override def validateToken(t: Lexer.Token): Boolean = true
}
