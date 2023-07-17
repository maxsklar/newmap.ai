package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Identifier, Number}
import ai.newmap.model.{EnvStatementParse, IterateIntoStatementParse, ParseElement, ParseTree, IdentifierParse, NaturalNumberParse}
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object IteratePath {

  val initState = State("iterate")
  val iterateIdentifier = State("iterateIdentifier")
  val iterateIndexSize = State("iterateIndexSize")

  val iterateIdentifierIdentifier = State("iterateIdentifierIdentifier")
  val iterateIdentifierIdentifierIdentifier = State("iterateIdentifierIdentifierIdentifier")
  val iterateEndState = new IterateEndState("iterateEndState")

  val iterateInitTransition = Transition(tokenValidator = TokenValidators.specificIdentifier("iterate"), nextState = initState)

  initState.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = iterateIdentifier))
  initState.addAcceptedTransition(Transition(tokenValidator = TokenValidators.number, nextState = iterateIndexSize))

  iterateIdentifier.addAcceptedTransition(Transition(tokenValidator = TokenValidators.specificIdentifier("into"), nextState = iterateIdentifierIdentifier))
  iterateIndexSize.addAcceptedTransition(Transition(tokenValidator = TokenValidators.specificIdentifier("into"), nextState = iterateIdentifierIdentifier))
  iterateIdentifierIdentifier.addAcceptedTransition(Transition(tokenValidator = TokenValidators.identifier, nextState = iterateIdentifierIdentifierIdentifier))
  iterateIdentifierIdentifierIdentifier.addAcceptedTransition(new IterateEndStateTransition(nextState = iterateEndState))
}

class IterateEndState(name: String) extends State(name, isEndState = true){

  var tokenOptions: Option[List[ParseElement]] = None
  override def reach(p: ListBuffer[ParseElement], ts:Seq[Lexer.Token]): Unit = {
    tokenOptions = Option(p.toList)
  }

  override def generateParseTree: Option[EnvStatementParse] = {
    val tokens = tokenOptions.get

    val iterableObject = tokens(1) match {
      case (a: Identifier) => IdentifierParse(a.s)
      case (a: Number) => NaturalNumberParse(a.i)
      case x => throw new Exception(s"Unexpected token for iterate parse token: $x")
    }

    Some(IterateIntoStatementParse(
      iterableObject,
      tokens(3).asInstanceOf[Identifier]
    ))
  }
}

class IterateEndStateTransition(nextState: State) extends Transition(TokenValidators.endOfInput, nextState)
