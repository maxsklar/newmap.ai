package ai.newmap.interpreter.parser

import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.parser.tokenUtils.TokenStream
import ai.newmap.model.{EmptyParse, EnvStatementParse, ExpressionOnlyStatementParse, ParseTree}
import ai.newmap.interpreter.parser.stateMachine.StateMachine
import ai.newmap.interpreter.parser.stateMachineConfig.{ExpressionPath, InitStatementState}
import ai.newmap.util.Outcome

object NewMapStateMachineParser {
  def apply(tokens: Seq[Lexer.Token]): Outcome[ParseTree, String] = {
    val result = (new StateMachine(ExpressionPath.InitState())).run(tokens)
    result.asInstanceOf[Outcome[ParseTree, String]]
  }

  def statementParse(tokens: Seq[Lexer.Token]): Outcome[EnvStatementParse, String] = {
    val result = (new StateMachine(InitStatementState())).run(tokens)
    result.asInstanceOf[Outcome[EnvStatementParse, String]]
  }
}
