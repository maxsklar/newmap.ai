package ai.newmap.interpreter.Parser

import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Parser.StateMachineConfig.StateMachineRunner
import ai.newmap.interpreter.Parser.TokenUtils.TokenStream
import ai.newmap.model.{EmptyParse, EnvStatementParse, ExpressionOnlyStatementParse, ParseTree}
import ai.newmap.util.Outcome


object NewMapStateMachineParser {

  //TODO:Add a parser function parameter to the parse function to call the respective state machines
  private def parse(tokens: Seq[Lexer.Token], emptyResult: Any): Outcome[Any, String] = {
    val tokenStream = new TokenStream(tokens, removeTokens = true)

    if (tokenStream.isEmpty) {
      ai.newmap.util.Success(emptyResult)
    } else {
      StateMachineRunner.run(tokens)
    }
  }

  def apply(tokens: Seq[Lexer.Token]): Outcome[ParseTree, String] = {
    val result = parse(tokens, EmptyParse)
    result.asInstanceOf[Outcome[ParseTree, String]]
  }

  def statementParse(tokens: Seq[Lexer.Token]): Outcome[EnvStatementParse, String] = {
    val result = parse(tokens, ExpressionOnlyStatementParse(EmptyParse()))
    result.asInstanceOf[Outcome[EnvStatementParse, String]]
  }
}
