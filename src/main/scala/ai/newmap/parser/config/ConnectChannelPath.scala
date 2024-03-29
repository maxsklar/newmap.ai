package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.parser.Lexer.Identifier
import ai.newmap.model.{ConnectChannelParse, EnvStatementParse}
import ai.newmap.util.{Failure, Outcome}

object ConnectChannelPath {
  case class ConnectChannelIdentifierIdentifier(firstId: String, secondId: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = Failure("Connect Channel only inputs 2 identifiers")

    override def generateOutput: Option[EnvStatementParse] = {
      Some(ConnectChannelParse(Identifier(firstId), Identifier(secondId)))
    }
  }

  case class ConnectChannelIdentifier(firstId: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => ConnectChannelIdentifierIdentifier(firstId, id))
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => ConnectChannelIdentifier(id))
    }
  }
}