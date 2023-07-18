package ai.newmap.interpreter.parser

import ai.newmap.interpreter.Lexer
import ai.newmap.model.{EnvStatementParse, ParseTree}
import ai.newmap.util.{Failure, Outcome, Success}

object NewMapParser {
  def apply(tokens: Seq[Lexer.Token]): Outcome[ParseTree, String] = {
    val parseTree = NewMapStateMachineParser(tokens)

    parseTree match {
      case Failure(v) =>
        if(v.equals("Unimplemented")) NewMapCombinatorParser(tokens)
        else ai.newmap.util.Failure(v)
      case Success(parseTree: ParseTree) => ai.newmap.util.Success(parseTree)
    }
  }

  def statementParse(tokens: Seq[Lexer.Token]): Outcome[EnvStatementParse, String] = {
    val statementParse = NewMapStateMachineParser.statementParse(tokens)

    statementParse match {
      case Failure(v) =>
        if (v.equals("Unimplemented")) {
          NewMapCombinatorParser.statementParse(tokens)
        }
        else Failure(v)
      case Success(envStatementParse: EnvStatementParse) => Success(envStatementParse)
    }
  }
}
