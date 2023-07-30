package ai.newmap.interpreter.parser.config

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Identifier, Number}
import ai.newmap.model.{EnvStatementParse, IterateIntoStatementParse, ParseElement, ParseTree, IdentifierParse, NaturalNumberParse}
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

object IteratePath {
  case class IterateIntoIdentifier(expression: ParseTree, id: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = Failure("Iterate statement is finished")

    override def generateOutput: Option[EnvStatementParse] = {
      Some(IterateIntoStatementParse(expression, Identifier(id)))
    }
  }

  case class IterateInto(expression: ParseTree) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => IterateIntoIdentifier(expression, id))
    }
  }

  case class InitState(val expressionState: ParseState[ParseTree] = ExpressionPath.InitState) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Identifier("into") => {
        Outcome(
          expressionState.generateOutput.map(exp => IterateInto(exp)),
          "Got into keyboard before expression is finished"
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