package ai.newmap.interpreter.parser.stateMachine

import ai.newmap.interpreter.Lexer.Token
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.util.{Failure, Success, Outcome}

object ParseStateUtils {
  def expectingIdentifier[OutT](token: Token, f: String => ParseState[OutT]): Outcome[ParseState[OutT], String] = {
    token match {
      case Identifier(id) => Success(f(id))
      case _ => Failure("Expected identifier, got " + token.toString)
    }
  }

  def expectingSpecificIdentifier[OutT](token: Token, keyword: String, result: ParseState[OutT]): Outcome[ParseState[OutT], String] = {
    token match {
      case Identifier(id) if (id == keyword) => Success(result)
      case _ => Failure("Expected keyword " + keyword)
    }
  }
}