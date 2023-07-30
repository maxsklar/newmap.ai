package ai.newmap.interpreter.parser.config

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Identifier, Symbol}
import ai.newmap.model._
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

object VersionedPath {
  case class VersionedIdentifierEqualsNew(id: String, val expressionState: ParseState[ParseTree] = ExpressionPath.InitState) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = for {
      newExpressionState <- expressionState.update(token)
    } yield {
      this.copy(expressionState = newExpressionState)
    }

    override def generateOutput: Option[EnvStatementParse] = {
      for {
        parseTree <- expressionState.generateOutput
      } yield {
        NewVersionedStatementParse(IdentifierParse(id), parseTree)
      }
    }
  }

  case class InitStateNewExpressionAsIdentifier(exp: ParseTree, id: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      Failure("New Versioned Items can only be stored in an identifier")
    }

    override def generateOutput: Option[EnvStatementParse] = {
      Some(NewVersionedStatementParse(IdentifierParse(id), exp))
    }
  }

  case class InitStateNewExpression(exp: ParseTree) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => InitStateNewExpressionAsIdentifier(exp, id))
    }
  }

  case class VersionedIdentifierEquals(id: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Identifier("new") => Success(VersionedIdentifierEqualsNew(id))
      case _ => Failure("Expected new, got " + token.toString)
    }
  }

  case class VersionedIdentifier(id: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Symbol("=") => Success(VersionedIdentifierEquals(id))
      case _ => Failure("Expected =, got " + token.toString)
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => VersionedIdentifier(id))
    }
  }

  /*
private def newVersionedStatement: Parser[NewVersionedStatementParse] = {
    Lexer.Identifier("new") ~ expressionListWithOperations ~ Lexer.Identifier("as") ~ identifier ^^ {
      case _ ~ exp ~ _ ~ id => {
        NewVersionedStatementParse(id, exp)
      }
    }
  }
  */

  case class InitStateNew(expressionState: ParseState[ParseTree] = ExpressionPath.InitState) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Identifier("as") => expressionState.generateOutput match {
        case Some(parseTree) => Success(InitStateNewExpression(parseTree))
        case None => {
          for {
            newExpressionState <- expressionState.update(token)
          } yield this.copy(expressionState = newExpressionState)
        }
      }
      case _ => {
        for {
          newExpressionState <- expressionState.update(token)
        } yield this.copy(expressionState = newExpressionState)
      }
    }
  }
}