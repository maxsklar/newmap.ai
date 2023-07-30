package ai.newmap.interpreter.parser.stateMachineConfig

import ai.newmap.interpreter.parser.stateMachine.{ParseState, ParseStateUtils}
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.Identifier
import ai.newmap.model.{DisconnectChannelParse, EnvStatementParse, ParseElement}
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

object DisconnectChannelPath {
  case class DisconnectChannelIdentifierIdentifier(firstId: String, secondId: String) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = Failure("Disconnect Channel only inputs 2 identifiers")

    override def generateOutput: Option[EnvStatementParse] = {
      Some(DisconnectChannelParse(Identifier(firstId), Identifier(secondId)))
    }
  }

  case class DisconnectChannelIdentifier(val firstId: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseStateUtils.expectingIdentifier(token, id => DisconnectChannelIdentifierIdentifier(firstId, id))
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseStateUtils.expectingIdentifier(token, id => DisconnectChannelIdentifier(id))
    }
  }
}