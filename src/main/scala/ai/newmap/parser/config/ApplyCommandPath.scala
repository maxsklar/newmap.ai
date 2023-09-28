package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.parser.Lexer.Symbol
import ai.newmap.model.{ApplyCommandStatementParse, EnvStatementParse, IdentifierParse, ParseTree}
import ai.newmap.util.{Failure, Success, Outcome}

object ApplyCommandPath {
  case class ApplyCommand(
    val identifier: String,
    val expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      for {
        newExpressionState <- expressionState.update(token)
      } yield {
        this.copy(expressionState = newExpressionState)
      }
    }

    override def generateOutput: Option[EnvStatementParse] = {
      for {
        parseTree <- expressionState.generateOutput
      } yield {
        ApplyCommandStatementParse(IdentifierParse(identifier), parseTree)
      }
    }
  }

  case class Colon(id: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Symbol(":") => Success(ApplyCommand(id))
      case _ => Failure("Expected =, got " + token.toString)
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => Colon(id))
    }
  }
}