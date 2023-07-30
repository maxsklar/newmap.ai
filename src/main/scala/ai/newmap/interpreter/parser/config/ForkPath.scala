package ai.newmap.interpreter.parser.config

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, ForkedVersionedStatementParse, IdentifierParse, ParseElement}
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

object ForkPath {
  case class ForkIdentifierAsIdentifier(firstId: String, secondId: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = Failure("Fork can only take 2 identifiers")

    override def generateOutput: Option[EnvStatementParse] = {
      Some(ForkedVersionedStatementParse(Identifier(secondId), Identifier(firstId)))
    }
  }

  case class ForkIdentifierAs(firstId: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => ForkIdentifierAsIdentifier(firstId, id))
    }
  }

  case class ForkIdentifier(id: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingSpecificIdentifier(token, "as", ForkIdentifierAs(id))
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => ForkIdentifier(id))
    }
  }
}