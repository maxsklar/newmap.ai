package ai.newmap.interpreter

import ai.newmap.model._
import scala.util.parsing.combinator.RegexParsers
import ai.newmap.util.{Outcome, Success, Failure}

object Lexer extends RegexParsers {
  sealed trait Token
  
  case class Enc(symbol: EnclosureSymbol, isOpen: Boolean) extends Token
  case class Identifier(id: String) extends Token
  case class Number(i: Long) extends Token
  case class Symbol(s: String) extends Token
  case class DQuote(s: String) extends Token
  case class Comment(s: String) extends Token

  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def identifier: Parser[Identifier] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => Identifier(str) }
  }

  def number: Parser[Number] = {
    "0|([1-9][0-9]*)".r ^^ { str => Number(str.toLong) }
  }

  def symbol: Parser[Symbol] = {
    "[\\.,:`=~><\\|][\\.,:`=~><\\|]*".r ^^ { str => Symbol(str)}
  }

  def dquote: Parser[DQuote] = {
    "\"([^\"]*)\"".r ^^ { str => DQuote(str) }
  }

  def comment: Parser[Comment] = {
    "//.*".r ^^ { str => Comment(str.drop(2)) }
  }

  def enclosure: Parser[Enc] = {
    "[\\(\\[\\{\\}\\]\\)]".r ^^ { str => {
      str match {
        case "(" => Enc(Paren, isOpen = true)
        case ")" => Enc(Paren, isOpen = false)
        case "[" => Enc(SquareBracket, isOpen = true)
        case "]" => Enc(SquareBracket, isOpen = false)
        case "{" => Enc(CurlyBrace, isOpen = true)
        case "}" => Enc(CurlyBrace, isOpen = false)
      }
    }}
  }

  def tokens: Parser[List[Token]] = {
    phrase(rep1(identifier | number | symbol | dquote | enclosure | comment )) ^^ { rawTokens =>
      rawTokens
    }
  }

  def apply(code: String): Outcome[List[Token], String] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => ai.newmap.util.Failure(msg)
      case Success(result, next) => ai.newmap.util.Success(result)
    }
  }
}