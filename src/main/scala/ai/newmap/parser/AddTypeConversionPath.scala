package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.parser.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, AddTypeConversionParse, ParseTree}
import ai.newmap.util.{Failure, Outcome}

object AddTypeConversionPath {
  case class ConvertToTypeWith(
    fromType: ParseTree,
    toType: ParseTree,
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
        conversionFunction <- expressionState.generateOutput
      } yield {
        AddTypeConversionParse(fromType, toType, conversionFunction)
      }
    }
  }

  case class ConvertTo(
    fromType: ParseTree,
    val expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Identifier("with") => {
        Outcome(
          expressionState.generateOutput.map(exp => ConvertToTypeWith(fromType, exp)),
          "Got `with` token before expression is finished"
        )
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

  case class InitState(val expressionState: ParseState[ParseTree] = ExpressionPath.InitState) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Identifier("to") => {
        Outcome(
          expressionState.generateOutput.map(exp => ConvertTo(exp)),
          "Got `to` token before expression is finished"
        )
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