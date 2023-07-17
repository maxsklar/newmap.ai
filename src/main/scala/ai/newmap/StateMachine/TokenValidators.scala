package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer._

object TokenValidators {
  val identifier: Token => Boolean = t => t match {
    case Identifier(_) => true
    case _ => false
  }

  def specificIdentifier(id: String): Token => Boolean = t => t match {
    case Identifier(identifier) if (identifier == id) => true
    case _ => false
  }

  val number: Token => Boolean = t => t match {
    case Number(_) => true
    case _ => false
  }

  val endOfInput: Token => Boolean = t => t == EndToken

  val none: Token => Boolean = t => true
}