package ai.newmap.interpreter.parser.config

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{ConnectChannelParse, EnvStatementParse, ParseElement}
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

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