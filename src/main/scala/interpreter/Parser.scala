package ai.newmap.interpreter

import ai.newmap.model._
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Reader, Position, NoPosition}
import ai.newmap.util.{Outcome, Success, Failure}

object NewMapParser extends Parsers {
  override type Elem = Lexer.Token

  class TokenReader(tokens: Seq[Lexer.Token]) extends Reader[Lexer.Token] {
    override def first: Lexer.Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Lexer.Token] = new TokenReader(tokens.tail)
  }

  private def naturalNumber: Parser[NaturalNumberParse] = {
    accept("number", { case Lexer.Number(i) => {
      NaturalNumberParse(i)
    }})
  }

  private def identifier: Parser[IdentifierParse] = {
    accept("identifier", { case Lexer.Identifier(id) => {
      IdentifierParse(id)
    }})
  }

  private def forcedId: Parser[IdentifierParse] = {
    Lexer.Tilda() ~ identifier ^^ {
      case _ ~ IdentifierParse(s, _) => IdentifierParse(s, force = true)
    }
  }

  private def expressionInParens: Parser[ParseTree] = {
    Lexer.Enc(Paren, true) ~ expressionList ~ Lexer.Enc(Paren, false) ^^ {
      case _ ~ exps ~ _ => {
        exps
      }
    }
  }

  private def enclosure: Parser[Enclosure] = {
    val pattern = {
      Lexer.Enc(Paren, true) ~
        repsep(expression ~ Lexer.Colon() ~ expressionList, Lexer.Comma()) ~
        Lexer.Enc(Paren, false)
    }
    pattern ^^ {
      case _ ~ items ~ _ => {
        val pairs = items.map(item => {
          (item._1._1-> item._2)
        })
        Enclosure(Paren, pairs.toVector)
      }
    }
  }

  private def lambdaParse: Parser[LambdaParse] = {
    enclosure ~ Lexer.Enc(CurlyBrace, true) ~ expressionList ~ Lexer.Enc(CurlyBrace, false) ^^ {
      case Enclosure(Paren, pairs) ~ _ ~ exp ~ _ => {
        LambdaParse(pairs, exp)
      }
    }
  }

  private def expression: Parser[ParseTree] = {
    expressionInParens | naturalNumber | identifier | forcedId | lambdaParse | enclosure
  }

  private def expressionList: Parser[ParseTree] = {
    rep1(expression) ^^ {
      case exps => exps reduceRight ApplyParse
    }
  }

  private def statement: Parser[StatementParse] = {
    Lexer.Identifier("val") ~ identifier ~ Lexer.Colon() ~ expressionList ~ Lexer.Equals() ~ expressionList ^^ {
      case _ ~ id ~ _ ~ typeExp ~ _ ~ exp => {
        StatementParse(ValStatement, id, typeExp, exp)
      }
    }
  }

  def apply(
    tokens: Seq[Lexer.Token]
  ): Outcome[ParseTree, String] = {
    val reader = new TokenReader(tokens)

    val program = phrase(expressionList)

    program(reader) match {
      case NoSuccess(msg, next) => ai.newmap.util.Failure(msg)
      case Success(result, next) => ai.newmap.util.Success(result)
    }
  }

  def statementParse(
    tokens: Seq[Lexer.Token]
  ): Outcome[StatementParse, String] = {
    val reader = new TokenReader(tokens)
    val program = phrase(statement)

    program(reader) match {
      case NoSuccess(msg, next) => ai.newmap.util.Failure(msg)
      case Success(result, next) => ai.newmap.util.Success(result)
    }
  }
}