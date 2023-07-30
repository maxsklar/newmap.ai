package ai.newmap.interpreter.parser.stateMachineConfig

import ai.newmap.interpreter.parser.stateMachine.{ParseState, ParseStateUtils}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Identifier, Symbol}
import ai.newmap.model._
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

object DataPath {
  case class DataIdentifierParams(
    val id: String,
    val expressionState: ParseState[ParseTree]
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
        NewParamTypeStatementParse(IdentifierParse(id), parseTree)
      }
    }
  }

  case class DataIdentifierEquals(
    val id: String,
    val expressionState: ParseState[ParseTree] = ExpressionPath.InitState()
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
        NewTypeStatementParse(IdentifierParse(id), parseTree)
      }
    }
  }

  case class DataIdentifier(id: String) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Symbol("=") => Success(DataIdentifierEquals(id))
      case _ => {
        for {
          params <- ExpressionPath.InitState().update(token)
        } yield {
          DataIdentifierParams(id, params)
        }
      }
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseStateUtils.expectingIdentifier(token, id => DataIdentifier(id))
    }
  }
}