package ai.newmap.interpreter.parser

import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Identifier, Token}
import ai.newmap.interpreter.parser.config.{ExpressionPath, InitStatementState}
import ai.newmap.model.{EnvStatementParse, ParseTree}
import ai.newmap.util.{Failure, Outcome, Success}

object NewMapParser {
  private def run[OutT](
    tokens: Seq[Lexer.Token],
    curState: ParseState[OutT]
  ): Outcome[OutT, String] = tokens match {
    case firstToken +: otherTokens => {
      for {
        newState <- curState.update(firstToken)
        result <- run(otherTokens, newState)
      } yield result
    }
    case Nil => {
      //println("end state: " + curState)
      Outcome(curState.generateOutput, "Unimplemented")
    }
  }

  def expressionParse(tokens: Seq[Lexer.Token]): Outcome[ParseTree, String] = {
    run(tokens, ExpressionPath.InitState)
  }

  def statementParse(tokens: Seq[Lexer.Token]): Outcome[EnvStatementParse, String] = {
    run(tokens, InitStatementState)
  }
}

case class NewMapCodeParserResponse(
  newParser: NewMapCodeParser,
  statementOutput: Option[EnvStatementParse] = None
)

case class NewMapCodeParser(curState: ParseState[EnvStatementParse] = InitStatementState) {
  def update(token: Lexer.Token): Outcome[NewMapCodeParserResponse, String] = {
    (token, curState.generateOutput) match {
      case (Lexer.NewLine() | Lexer.Symbol(";"), Some(statement)) => {
        Success(
          NewMapCodeParserResponse(NewMapCodeParser(), Some(statement))
        )
      }
      case _ => curState.update(token).map(newState => NewMapCodeParserResponse(NewMapCodeParser(newState)))
    }
  }
}

trait ParseState[OutT] {
  // Update tot he next state given the token
  def update(token: Lexer.Token): Outcome[ParseState[OutT], String]

  // If this generates the output, then this is an endState
  def generateOutput: Option[OutT] = None
}

object ParseState {
  def expectingIdentifier[OutT](token: Token, f: String => ParseState[OutT]): Outcome[ParseState[OutT], String] = {
    token match {
      case Identifier(id) => Success(f(id))
      case _ => Failure("Expected identifier, got " + token.toString)
    }
  }

  def expectingSpecificIdentifier[OutT](token: Token, keyword: String, result: ParseState[OutT]): Outcome[ParseState[OutT], String] = {
    token match {
      case Identifier(id) if (id == keyword) => Success(result)
      case _ => Failure("Expected keyword " + keyword)
    }
  }
}
