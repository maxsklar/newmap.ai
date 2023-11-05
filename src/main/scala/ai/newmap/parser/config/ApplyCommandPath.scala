package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.parser.Lexer.{Identifier, Symbol}
import ai.newmap.model.{ApplyCommandStatementParse, ApplyCustomCommandParse, EnvStatementParse, IdentifierParse, ParseTree}
import ai.newmap.util.{Failure, Success, Outcome}

object ApplyCommandPath {
  case class ApplyCustomCommandId(
    identifier: String,
    commandName: String,
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
        ApplyCustomCommandParse(IdentifierParse(identifier), IdentifierParse(commandName), parseTree)
      }
    }
  }

  case class ApplyCustomCommand(
    identifier: String
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => ApplyCustomCommandId(identifier, id))
    }
  }

  case class ApplyCommandColon(
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

  case class UpdateId(id: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Symbol(":") => Success(ApplyCommandColon(id))
      case Symbol(".") => Success(ApplyCustomCommand(id))
      case _ => Failure("Expected : or ., got " + token.toString)
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      if (token == Identifier("class")) {
        Success(UpdateClassPath.InitState())
      } else {
        ParseState.expectingIdentifier(token, id => UpdateId(id))
      }
    }
  }
}