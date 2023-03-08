package ai.newmap.interpreter

import ai.newmap.model.{EnvStatementParse, ParseTree}
import ai.newmap.util.Outcome

object NewMapStateMachineParser {
  def apply(
             tokens: Seq[Lexer.Token]
           ): Outcome[ParseTree, String] = {
    ai.newmap.util.Failure("Unimplemented")
  }

  def statementParse(
             tokens: Seq[Lexer.Token]
           ): Outcome[EnvStatementParse, String] = {
    ai.newmap.util.Failure("Unimplemented")
  }

}
