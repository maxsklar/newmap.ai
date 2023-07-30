package ai.newmap.interpreter.parser.config

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, IdentifierParse, ParseElement, ParseTree, WriteToChannelParse}
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

object WritePath {
  case class WriteIdenfitier(val id: String, val expressionState: ParseState[ParseTree] = ExpressionPath.InitState) extends ParseState[EnvStatementParse] {
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
        WriteToChannelParse(IdentifierParse(id), parseTree)
      }
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => WriteIdenfitier(id))
    }
  }
}