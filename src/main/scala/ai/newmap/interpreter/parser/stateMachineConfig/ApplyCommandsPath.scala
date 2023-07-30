package ai.newmap.interpreter.parser.stateMachineConfig

import ai.newmap.interpreter.parser.stateMachine.{ParseState, ParseStateUtils}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ApplyCommandsStatementParse, EnvStatementParse, IdentifierParse, ParseElement, ParseTree}
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

object ApplyCommandsPath {
  case class ApplyCommands(
    val identifier: String,
    val expressionState: ParseState[ParseTree] = ExpressionPath.InitState()
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
      ParseStateUtils.expectingIdentifier(token, id => ApplyCommands(id))
    }
  }
}
