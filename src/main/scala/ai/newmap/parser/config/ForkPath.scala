package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.parser.Lexer.Identifier
import ai.newmap.model.{EnvStatementParse, ForkedVersionedStatementParse}
import ai.newmap.util.{Failure, Outcome}

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