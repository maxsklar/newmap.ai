package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.parser.Lexer.Identifier
import ai.newmap.model.{DisconnectChannelParse, EnvStatementParse}
import ai.newmap.util.{Failure, Outcome}

object DisconnectChannelPath {
  case class DisconnectChannelIdentifierIdentifier(firstId: String, secondId: String) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = Failure("Disconnect Channel only inputs 2 identifiers")

    override def generateOutput: Option[EnvStatementParse] = {
      Some(DisconnectChannelParse(Identifier(firstId), Identifier(secondId)))
    }
  }

  case class DisconnectChannelIdentifier(val firstId: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => DisconnectChannelIdentifierIdentifier(firstId, id))
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => DisconnectChannelIdentifier(id))
    }
  }
}