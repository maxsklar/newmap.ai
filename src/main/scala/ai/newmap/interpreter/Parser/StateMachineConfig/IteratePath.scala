package ai.newmap.interpreter.Parser.StateMachineConfig

import ai.newmap.StateMachine.{State, Transition}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Identifier, Number}
import ai.newmap.model.{EnvStatementParse, IterateIntoStatementParse, ParseElement, ParseTree, IdentifierParse, NaturalNumberParse}
import ai.newmap.StateMachine.TokenValidators
import scala.collection.mutable.ListBuffer

object IteratePath {

  val iterateEndState = new IterateEndState("iterateEndState")

  val iterateIdentifierIdentifierIdentifier = State("iterateIdentifierIdentifierIdentifier", Vector(
    new IterateEndStateTransition(nextState = iterateEndState)
  ))

  val iterateIdentifierIdentifier = State("iterateIdentifierIdentifier", Vector(
    Transition(TokenValidators.identifier, iterateIdentifierIdentifierIdentifier)
  ))

  val iterateIndexSize = State("iterateIndexSize", Vector(
    Transition(TokenValidators.specificIdentifier("into"), iterateIdentifierIdentifier)
  ))

  val iterateIdentifier = State("iterateIdentifier", Vector(
    Transition(TokenValidators.specificIdentifier("into"), iterateIdentifierIdentifier)
  ))

  val initState = State("iterate", Vector(
    Transition(TokenValidators.identifier, iterateIdentifier),
    Transition(TokenValidators.number, iterateIndexSize)
  ))
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
