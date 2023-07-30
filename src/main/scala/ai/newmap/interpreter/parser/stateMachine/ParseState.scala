package ai.newmap.interpreter.parser.stateMachine

import ai.newmap.interpreter._
import ai.newmap.model.{EnvStatementParse, ParseElement}
import ai.newmap.util.{Failure, Success, Outcome}

trait ParseState[OutT] {
  // Update tot he next state given the token
  def update(token: Lexer.Token): Outcome[ParseState[OutT], String]

  // If this generates the output, then this is an endState
  def generateOutput: Option[OutT] = None
}