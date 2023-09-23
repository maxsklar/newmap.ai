package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.model._
import ai.newmap.util.Outcome

object TypeClassPath {
  case class TypeClassIdentifier(
    id: String,
    expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = for {
      newExpressionState <- expressionState.update(token)
    } yield {
      this.copy(expressionState = newExpressionState)
    }

    override def generateOutput: Option[EnvStatementParse] = {
      for {
        parseTree <- expressionState.generateOutput
      } yield {
        NewTypeClassStatementParse(IdentifierParse(id), parseTree)
      }
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => TypeClassIdentifier(id))
    }
  }
}