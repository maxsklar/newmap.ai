package ai.newmap.interpreter.parser.config

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Identifier, Symbol}
import ai.newmap.model._
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

object ValPath {
  case class ValIdentifierEquals(
    prefix: StatementPrefix,
    id: String,
    typeExp: Option[ParseTree] = None,
    expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      for {
        newExpressionState <- expressionState.update(token)
      } yield this.copy(expressionState = newExpressionState)
    }

    override def generateOutput: Option[EnvStatementParse] = {
      for {
        parseTree <- expressionState.generateOutput
      } yield {
        typeExp match {
          case Some(t) => FullStatementParse(prefix, IdentifierParse(id), t, parseTree)
          case None => InferredTypeStatementParse(prefix, IdentifierParse(id), parseTree)
        }
      }
    }
  }

  case class ValIdentifierColon(
    prefix: StatementPrefix,
    id: String,
    expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      val currentTypeExpression = expressionState.generateOutput
      token match {
        case Symbol("=") if (currentTypeExpression.nonEmpty) => {
          Success(ValIdentifierEquals(prefix, id, currentTypeExpression))
        }
        case _ => {
          for {
            newExpressionState <- expressionState.update(token)
          } yield {
            this.copy(expressionState = newExpressionState)
          }
        }
      }
    }
  }

  case class ValIdentifier(prefix: StatementPrefix, id: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Symbol("=") => Success(ValIdentifierEquals(prefix, id))
      case Symbol(":") => Success(ValIdentifierColon(prefix, id))
      case _ => Failure("Expected =, got " + token.toString)
    }
  }

  case class InitState(prefix: StatementPrefix) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => ValIdentifier(prefix, id))
    }
  }
}