package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.model.{ApplyCommandsStatementParse, EnvStatementParse, IdentifierParse, ParseTree}
import ai.newmap.util.Outcome

object ApplyCommandsPath {
  case class ApplyCommands(
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
        ApplyCommandsStatementParse(IdentifierParse(identifier), parseTree)
      }
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => ApplyCommands(id))
    }
  }
}
